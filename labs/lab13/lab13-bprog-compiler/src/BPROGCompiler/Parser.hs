module BPROGCompiler.Parser
  ( -- * Parsing functions
    parseProgram
  , parseLine
  , parseToken
  ) where

import BPROGCompiler.Types
import BPROGCompiler.Error

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit, isSpace)
import Text.Read (readMaybe)

-- | Parse a full BPROG program into an AST
parseProgram :: Text -> Compiler [BPROGExpr]
parseProgram program = do
  -- Split into tokens using 'words'
  let tokens = T.words program
  
  -- Skip comments (lines starting with '#')
  let filteredTokens = filter (not . T.isPrefixOf (T.pack "#")) tokens
  
  -- Parse the tokens into expressions
  if null filteredTokens
    then return []
    else parseTokensToExpr filteredTokens []

-- | Parse a single line of BPROG code
parseLine :: Text -> Compiler [BPROGExpr]
parseLine line = do
  -- Skip comments
  let lineWithoutComments = T.takeWhile (/= '#') line
  
  -- Split into tokens using 'words'
  let tokens = T.words lineWithoutComments
  
  -- Parse the tokens into expressions
  if null tokens
    then return []
    else parseTokensToExpr tokens []

-- | Parse a single token into a Token type
parseToken :: Text -> Compiler Token
parseToken token = case token of
  -- Brackets and braces
  t | t == T.pack "[" -> return ListStartToken
  t | t == T.pack "]" -> return ListEndToken
  t | t == T.pack "{" -> return QuotationStartToken
  t | t == T.pack "}" -> return QuotationEndToken

  -- Boolean literals
  t | t == T.pack "True" -> return $ BoolToken True
  t | t == T.pack "False" -> return $ BoolToken False

  -- Operators
  t | t == T.pack "+" -> return $ OperatorToken (T.pack "+")
  t | t == T.pack "-" -> return $ OperatorToken (T.pack "-")
  t | t == T.pack "*" -> return $ OperatorToken (T.pack "*")
  t | t == T.pack "/" -> return $ OperatorToken (T.pack "/")
  t | t == T.pack "div" -> return $ OperatorToken (T.pack "div")
  t | t == T.pack "<" -> return $ OperatorToken (T.pack "<")
  t | t == T.pack ">" -> return $ OperatorToken (T.pack ">")
  t | t == T.pack "==" -> return $ OperatorToken (T.pack "==")
  t | t == T.pack "&&" -> return $ OperatorToken (T.pack "&&")
  t | t == T.pack "||" -> return $ OperatorToken (T.pack "||")
  t | t == T.pack "not" -> return $ OperatorToken (T.pack "not")

  -- Stack operations
  t | t == T.pack "dup" -> return $ OperatorToken (T.pack "dup")
  t | t == T.pack "swap" -> return $ OperatorToken (T.pack "swap")
  t | t == T.pack "pop" -> return $ OperatorToken (T.pack "pop")

  -- List operations
  t | t == T.pack "head" -> return $ OperatorToken (T.pack "head")
  t | t == T.pack "tail" -> return $ OperatorToken (T.pack "tail")
  t | t == T.pack "empty" -> return $ OperatorToken (T.pack "empty")
  t | t == T.pack "length" -> return $ OperatorToken (T.pack "length")
  t | t == T.pack "cons" -> return $ OperatorToken (T.pack "cons")
  t | t == T.pack "append" -> return $ OperatorToken (T.pack "append")
  t | t == T.pack "map" -> return $ OperatorToken (T.pack "map")
  t | t == T.pack "foldl" -> return $ OperatorToken (T.pack "foldl")
  t | t == T.pack "each" -> return $ OperatorToken (T.pack "each")

  -- Control flow
  t | t == T.pack "if" -> return $ OperatorToken (T.pack "if")
  t | t == T.pack "loop" -> return $ OperatorToken (T.pack "loop")
  t | t == T.pack "times" -> return $ OperatorToken (T.pack "times")
  t | t == T.pack "exec" -> return $ OperatorToken (T.pack "exec")

  -- Assignment
  t | t == T.pack ":=" -> return $ OperatorToken (T.pack ":=")
  t | t == T.pack "fun" -> return $ OperatorToken (T.pack "fun")

  -- String parsing
  t | t == T.pack "parseInteger" -> return $ OperatorToken (T.pack "parseInteger")
  t | t == T.pack "parseFloat" -> return $ OperatorToken (T.pack "parseFloat")
  t | t == T.pack "words" -> return $ OperatorToken (T.pack "words")
  
  -- String literal
  t | T.isPrefixOf (T.pack "\"") t && T.isSuffixOf (T.pack "\"") t ->
    return $ StringToken $ T.drop 1 $ T.dropEnd 1 t
  
  -- Number literal
  t | all isDigit (T.unpack t) -> 
    return $ IntToken $ read $ T.unpack t
  
  -- Negative integer
  t | T.isPrefixOf (T.pack "-") t && all isDigit (T.unpack $ T.drop 1 t) ->
    return $ IntToken $ read $ T.unpack t
  
  -- Float literal
  t | isFloatLiteral t ->
    case readMaybe (T.unpack t) of
      Just n -> return $ FloatToken n
      Nothing -> throwError $ ParserErr $ InvalidNumber t
  
  -- Default to symbol
  _ -> return $ SymbolToken token

-- | Check if a token is a float literal
isFloatLiteral :: Text -> Bool
isFloatLiteral t = 
  T.count (T.pack ".") t == 1 && 
  all (\c -> isDigit c || c == '.' || c == '-') (T.unpack t) &&
  (T.head t /= '.' && T.last t /= '.')

-- | Parse tokens into expressions
parseTokensToExpr :: [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
parseTokensToExpr [] exprs = return $ reverse exprs
parseTokensToExpr (t:ts) exprs = do
  token <- parseToken t
  case token of
    IntToken n -> 
      parseTokensToExpr ts (IntLit n : exprs)
    
    FloatToken n -> 
      parseTokensToExpr ts (FloatLit n : exprs)
    
    BoolToken b -> 
      parseTokensToExpr ts (BoolLit b : exprs)
    
    StringToken s -> 
      parseTokensToExpr ts (StringLit s : exprs)
    
    SymbolToken s -> 
      parseTokensToExpr ts (SymbolRef s : exprs)
    
    ListStartToken -> do
      (listItems, remainingTokens) <- parseListItems ts []
      parseTokensToExpr remainingTokens (ListLit listItems : exprs)
    
    QuotationStartToken -> do
      (blockItems, remainingTokens) <- parseBlockItems ts []
      parseTokensToExpr remainingTokens (QuotationLit blockItems : exprs)
    
    OperatorToken op ->
      case T.unpack op of
        -- Binary operators
        "+" -> handleBinaryOp (T.pack "+") ts exprs
        "-" -> handleBinaryOp (T.pack "-") ts exprs
        "*" -> handleBinaryOp (T.pack "*") ts exprs
        "/" -> handleBinaryOp (T.pack "/") ts exprs
        "div" -> handleBinaryOp (T.pack "div") ts exprs
        "<" -> handleBinaryOp (T.pack "<") ts exprs
        ">" -> handleBinaryOp (T.pack ">") ts exprs
        "==" -> handleBinaryOp (T.pack "==") ts exprs
        "&&" -> handleBinaryOp (T.pack "&&") ts exprs
        "||" -> handleBinaryOp (T.pack "||") ts exprs

        -- Unary operators
        "not" -> handleUnaryOp (T.pack "not") ts exprs

        -- Stack operations
        "dup" -> parseTokensToExpr ts (StackOp (T.pack "dup") : exprs)
        "swap" -> parseTokensToExpr ts (StackOp (T.pack "swap") : exprs)
        "pop" -> parseTokensToExpr ts (StackOp (T.pack "pop") : exprs)

        -- Control flow operations
        "if" -> handleIfThenElse ts exprs
        "loop" -> handleLoop ts exprs
        "times" -> handleTimes ts exprs

        -- Assignment operations
        ":=" -> handleAssignment ts exprs
        "fun" -> handleFunctionDef ts exprs

        -- List operations
        "head" -> parseTokensToExpr ts (ListOp (T.pack "head") [] : exprs)
        "tail" -> parseTokensToExpr ts (ListOp (T.pack "tail") [] : exprs)
        "empty" -> parseTokensToExpr ts (ListOp (T.pack "empty") [] : exprs)
        "length" -> parseTokensToExpr ts (ListOp (T.pack "length") [] : exprs)
        "cons" -> parseTokensToExpr ts (ListOp (T.pack "cons") [] : exprs)
        "append" -> parseTokensToExpr ts (ListOp (T.pack "append") [] : exprs)
        "map" -> handleListHOF (T.pack "map") ts exprs
        "foldl" -> handleListHOF (T.pack "foldl") ts exprs
        "each" -> handleListHOF (T.pack "each") ts exprs

        -- Parsing operations
        "parseInteger" -> parseTokensToExpr ts (UnaryOp (T.pack "parseInteger") (StringLit T.empty) : exprs)
        "parseFloat" -> parseTokensToExpr ts (UnaryOp (T.pack "parseFloat") (StringLit T.empty) : exprs)
        "words" -> parseTokensToExpr ts (UnaryOp (T.pack "words") (StringLit T.empty) : exprs)
        
        -- Default case
        _ -> throwError $ ParserErr $ UnexpectedToken t
    
    _ -> throwError $ ParserErr $ UnexpectedToken t
  
  where
    -- Parse list items
    parseListItems :: [Text] -> [BPROGExpr] -> Compiler ([BPROGExpr], [Text])
    parseListItems [] _ = throwError $ ParserErr IncompleteList
    parseListItems (t:rest) items | t == T.pack "]" = return (reverse items, rest)
    parseListItems tokens items = do
      -- Parse the first token
      firstItemExprs <- parseLine (head tokens)
      if null firstItemExprs
        then parseListItems (tail tokens) items
        else parseListItems (tail tokens) (head firstItemExprs : items)
    
    -- Parse code block items
    parseBlockItems :: [Text] -> [BPROGExpr] -> Compiler ([BPROGExpr], [Text])
    parseBlockItems [] _ = throwError $ ParserErr IncompleteQuotation
    parseBlockItems (t:rest) items | t == T.pack "}" = return (reverse items, rest)
    parseBlockItems tokens items = do
      -- Parse the first token
      firstItemExprs <- parseLine (head tokens)
      if null firstItemExprs
        then parseBlockItems (tail tokens) items
        else parseBlockItems (tail tokens) (head firstItemExprs : items)
    
    -- Handle binary operations
    handleBinaryOp :: Text -> [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleBinaryOp op ts exprs =
      if length exprs < 2
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let right = head exprs
          let left = head (tail exprs)
          let rest = tail (tail exprs)
          parseTokensToExpr ts (BinaryOp op left right : rest)

    -- Handle unary operations
    handleUnaryOp :: Text -> [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleUnaryOp op ts exprs =
      if null exprs
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let operand = head exprs
          let rest = tail exprs
          parseTokensToExpr ts (UnaryOp op operand : rest)
    
    -- Handle if-then-else
    handleIfThenElse :: [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleIfThenElse ts exprs =
      if null exprs
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let condition = head exprs
          let rest = tail exprs
          -- TODO: Parse the 'then' and 'else' blocks
          parseTokensToExpr ts (IfThenElse condition [] [] : rest)
    
    -- Handle loop
    handleLoop :: [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleLoop ts exprs =
      if null exprs
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let condition = head exprs
          let rest = tail exprs
          -- TODO: Parse the loop body
          parseTokensToExpr ts (Loop condition [] : rest)
    
    -- Handle times
    handleTimes :: [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleTimes ts exprs =
      if null exprs
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let count = head exprs
          let rest = tail exprs
          -- TODO: Parse the body to repeat
          parseTokensToExpr ts (Times count [] : rest)
    
    -- Handle assignment
    handleAssignment :: [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleAssignment ts exprs =
      if length exprs < 2
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let value = head exprs
          case head (tail exprs) of
            SymbolRef name -> 
              parseTokensToExpr ts (VarAssign name value : tail (tail exprs))
            _ -> 
              throwError $ TypeCheckErr $ InvalidOperandType (T.pack ":=") (T.pack "Symbol")
    
    -- Handle function definition
    handleFunctionDef :: [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleFunctionDef ts exprs =
      if length exprs < 2
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let body = head exprs
          case head (tail exprs) of
            SymbolRef name -> 
              case body of
                QuotationLit bodyExprs ->
                  parseTokensToExpr ts (FuncDef name bodyExprs : tail (tail exprs))
                _ ->
                  throwError $ TypeCheckErr $ InvalidOperandType (T.pack "fun") (T.pack "Quotation")
            _ -> 
              throwError $ TypeCheckErr $ InvalidOperandType (T.pack "fun") (T.pack "Symbol")
    
    -- Handle higher-order list functions (map, foldl, each)
    handleListHOF :: Text -> [Text] -> [BPROGExpr] -> Compiler [BPROGExpr]
    handleListHOF op ts exprs =
      if length exprs < 2
        then throwError $ TypeCheckErr StackUnderflow
        else do
          let fnOrValue = head exprs
          let list = head (tail exprs)
          let rest = tail (tail exprs)
          parseTokensToExpr ts (ListOp op [fnOrValue] : rest)