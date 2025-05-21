module BPROG.Parser
  ( -- * Parser
    parseTokens
  , parseToken
  , parseLine
  , parseProgram
  ) where

import BPROG.Types
import BPROG.Error

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char
import Control.Monad.Trans.Except (ExceptT, throwE)
import Text.Read (readMaybe)

-- | Parse a line of BPROG code into tokens
parseLine :: Text -> Interpreter [Text]
parseLine line = do
  let tokens = tokenize line
  return tokens

-- | Parse a BPROG program (multiple lines) into tokens
parseProgram :: Text -> Interpreter [Text]
parseProgram program = do
  let tokens = tokenize program
  return tokens

-- | Parse tokens into Values
parseTokens :: [Text] -> Interpreter [Value]
parseTokens = mapM parseToken

-- | Parse a single token into a Value
parseToken :: Text -> Interpreter Value
parseToken token
  -- Integer literal (strict check)
  | not (T.null token) && T.all Char.isDigit token =
    return $ IntVal (read (T.unpack token))
      
  -- Negative Integer literal (strict check with only a minus at start)
  | not (T.null token) && T.length token > 1 && T.head token == '-' && T.all Char.isDigit (T.tail token) =
    return $ IntVal (read (T.unpack token))
      
  -- Float literal with optional negative sign
  | not (T.null token) && T.any (== '.') token && 
    T.all (\c -> Char.isDigit c || c == '.' || (c == '-' && T.head token == '-' && T.length token > 1)) token =
    return $ FloatVal (read (T.unpack token))
      
  -- Boolean literals
  | token == T.pack "True" = return $ BoolVal True
  | token == T.pack "False" = return $ BoolVal False
  
  -- String literal
  | T.take 1 token == T.pack "\"" && T.takeEnd 1 token == T.pack "\"" =
    return $ StringVal (T.strip $ T.dropEnd 1 (T.drop 1 token))
  
  -- Quote a Symbol
  | otherwise = return $ SymbolVal token

-- | Tokenize BPROG code into tokens
tokenize :: Text -> [Text]
tokenize code = filter (not . T.null) $ removeComments $ tokenizeImpl code

-- | Remove comments from tokenized code
removeComments :: [Text] -> [Text]
removeComments [] = []
removeComments (token:rest)
  -- Skip tokens starting with #
  | not (T.null token) && T.head token == '#' = 
      let remainingTokens = dropWhile (\t -> not (T.isInfixOf (T.pack "\n") t)) rest
      in removeComments remainingTokens
  | otherwise = token : removeComments rest

-- | Implementation of the tokenizer
tokenizeImpl :: Text -> [Text]
tokenizeImpl code
  | T.null code = []
  | otherwise =
    let (token, rest) = extractToken code
    in token : tokenizeImpl rest

-- | Extract the first token from a Text
extractToken :: Text -> (Text, Text)
extractToken code
  -- Skip whitespace
  | T.null code = (T.empty, T.empty)
  | Char.isSpace (T.head code) = extractToken (T.dropWhile Char.isSpace code)
  
  -- Skip comments (lines starting with #)
  | T.head code == '#' = let (_, r) = T.breakOn (T.pack "\n") code
                         in (T.empty, if T.null r then T.empty else T.tail r)
  
  -- Handle string literals
  | T.head code == '"' =
    let (str, rest) = T.breakOn (T.pack "\"") (T.tail code)
    in if T.null rest
       then (T.cons '"' str, T.empty) -- Incomplete string
       else (T.cons '"' (T.snoc str '"'), T.tail rest)
  
  -- Handle list literals
  | T.head code == '[' = (T.singleton '[', T.tail code)
  | T.head code == ']' = (T.singleton ']', T.tail code)
  
  -- Handle quotation (code block) literals
  | T.head code == '{' = (T.singleton '{', T.tail code)
  | T.head code == '}' = (T.singleton '}', T.tail code)
  
  -- Handle comparison operators and other special symbols
  | T.head code == '>' = (T.singleton '>', T.tail code)
  | T.head code == '<' = (T.singleton '<', T.tail code)
  | T.head code == '=' && T.length code > 1 && T.head (T.tail code) == '=' = (T.pack "==", T.drop 2 code)
  | T.head code == '=' = (T.singleton '=', T.tail code)
    
  -- Handle normal tokens (delimited by whitespace)
  | otherwise =
    let (token, rest) = T.break (\c -> Char.isSpace c || c == '{' || c == '}' || c == '[' || c == ']' || c == '>' || c == '<' || c == '=') code
    in (token, rest)