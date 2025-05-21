module BPROGCompiler.Compiler
  ( -- * Compiler functions
    compileExpr
  , compileProgram
  , inferType
  , typeCheck
  , assignMemoryLocations
  ) where

import BPROGCompiler.Types
import BPROGCompiler.Error

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)

-- | Compile a single expression
compileExpr :: BPROGExpr -> CompilerState -> Compiler (Text, CompilerState)
compileExpr expr state = case expr of
  -- Integer literal
  IntLit n -> do
    let watCode = T.pack $ "    ;; Push integer onto stack\n    i32.const " ++ show n ++ "\n"
    return (watCode, state)
  
  -- Float literal
  FloatLit n -> do
    let watCode = T.pack $ "    ;; Push float onto stack\n    f32.const " ++ show n ++ "\n"
    return (watCode, state)
  
  -- Boolean literal
  BoolLit b -> do
    let value = if b then 1 else 0
    let watCode = T.pack $ "    ;; Push boolean onto stack\n    i32.const " ++ show value ++ "\n"
    return (watCode, state)
  
  -- String literal
  StringLit str -> do
    -- Allocate memory for the string
    let (addr, newState) = allocateString str state
    let watCode = T.pack $ "    ;; Push string address onto stack\n    i32.const " ++ show addr ++ " ;; Address of string\n"
    return (watCode, newState)
  
  -- Symbol reference
  SymbolRef name -> compileSymbolRef name state
  
  -- List literal
  ListLit items -> compileListLiteral items state
  
  -- Quotation literal
  QuotationLit _ -> do
    -- Quotations are more complex, we'll handle them in a separate function
    throwError $ CodeGenErr $ UnsupportedOperation (T.pack "Quotation literals")
  
  -- Binary operation
  BinaryOp op left right -> compileBinaryOp op left right state
  
  -- Unary operation
  UnaryOp op operand -> compileUnaryOp op operand state
  
  -- Stack operation
  StackOp op -> compileStackOp op state
  
  -- Other expression types
  _ -> throwError $ CodeGenErr $ UnsupportedOperation (T.pack "Operation not implemented yet")

-- | Compile a list of expressions (a program)
compileProgram :: [BPROGExpr] -> CompilerState -> Compiler (Text, CompilerState)
compileProgram [] state = return (T.empty, state)
compileProgram (expr:exprs) state = do
  (code, newState) <- compileExpr expr state
  (restCode, finalState) <- compileProgram exprs newState
  return (code `T.append` restCode, finalState)

-- | Allocate memory for a string and return its address
allocateString :: Text -> CompilerState -> (Int, CompilerState)
allocateString str state =
  let layout = memoryLayout state
      strTable = stringTable layout
      -- Check if string already exists in memory
      existingAddr = lookup str strTable
  in case existingAddr of
    Just addr -> (addr, state)  -- Reuse existing string
    Nothing -> 
      -- Calculate new address for the string
      let strLen = T.length str
          addr = currentAddress layout
          -- Update memory layout
          newLayout = layout {
            currentAddress = addr + strLen + 1,  -- +1 for null terminator
            stringTable = (str, addr) : strTable
          }
          newState = state { memoryLayout = newLayout }
      in (addr, newState)

-- | Compile a symbol reference
compileSymbolRef :: Text -> CompilerState -> Compiler (Text, CompilerState)
compileSymbolRef name state =
  case lookup name (symbolTable state) of
    Just (_, Just addr) -> do
      -- Symbol is a global variable, load its value
      let watCode = T.pack $ "    ;; Load variable " ++ T.unpack name ++ "\n" ++
                            "    global.get $" ++ T.unpack name ++ "\n"
      return (watCode, state)
    Just (_, Nothing) -> do
      -- Symbol is a function, we'll call it
      let watCode = T.pack $ "    ;; Call function " ++ T.unpack name ++ "\n" ++
                            "    call $" ++ T.unpack name ++ "\n"
      return (watCode, state)
    Nothing -> do
      -- Unknown symbol
      throwError $ TypeCheckErr $ UndefinedSymbol name

-- | Compile a list literal
compileListLiteral :: [BPROGExpr] -> CompilerState -> Compiler (Text, CompilerState)
compileListLiteral items state = do
  -- Create an empty list
  let watCode1 = T.pack "    ;; Create empty list\n    call $create_list\n"
  
  -- Compile each item and add it to the list
  (itemsCode, newState) <- foldM compileListItem (T.empty, state) items
  
  return (watCode1 `T.append` itemsCode, newState)
  where
    compileListItem :: (Text, CompilerState) -> BPROGExpr -> Compiler (Text, CompilerState)
    compileListItem (code, st) item = do
      -- Compile the item
      (itemCode, newState) <- compileExpr item st
      
      -- Add the item to the list
      let addCode = T.pack "    ;; Add item to list\n    call $list_add\n"
      
      return (code `T.append` itemCode `T.append` addCode, newState)

-- | Compile a binary operation
compileBinaryOp :: Text -> BPROGExpr -> BPROGExpr -> CompilerState -> Compiler (Text, CompilerState)
compileBinaryOp op left right state = do
  -- First compile the left operand
  (leftCode, state1) <- compileExpr left state
  
  -- Then compile the right operand
  (rightCode, state2) <- compileExpr right state1
  
  -- Infer types of the operands
  leftType <- inferType left state
  rightType <- inferType right state
  
  -- Generate the appropriate WebAssembly operation based on types
  opCode <- getWasmBinaryOp op leftType rightType
  
  let watCode = leftCode `T.append` rightCode `T.append` 
                T.pack ("    ;; " ++ T.unpack op ++ " operation\n" ++ 
                        "    " ++ T.unpack opCode ++ "\n")
  
  return (watCode, state2)

-- | Get the WebAssembly binary operation based on BPROG operation and operand types
getWasmBinaryOp :: Text -> BPROGType -> BPROGType -> Compiler Text
getWasmBinaryOp op leftType rightType = do
  let opStr = T.unpack op
  
  case (opStr, leftType, rightType) of
    -- Addition
    ("+", IntType, IntType) -> return (T.pack "i32.add")
    ("+", FloatType, FloatType) -> return (T.pack "f32.add")
    ("+", IntType, FloatType) -> return (T.pack "f32.add") -- After implicit conversion
    ("+", FloatType, IntType) -> return (T.pack "f32.add") -- After implicit conversion
    
    -- Subtraction
    ("-", IntType, IntType) -> return (T.pack "i32.sub")
    ("-", FloatType, FloatType) -> return (T.pack "f32.sub")
    ("-", IntType, FloatType) -> return (T.pack "f32.sub") -- After implicit conversion
    ("-", FloatType, IntType) -> return (T.pack "f32.sub") -- After implicit conversion
    
    -- Multiplication
    ("*", IntType, IntType) -> return (T.pack "i32.mul")
    ("*", FloatType, FloatType) -> return (T.pack "f32.mul")
    ("*", IntType, FloatType) -> return (T.pack "f32.mul") -- After implicit conversion
    ("*", FloatType, IntType) -> return (T.pack "f32.mul") -- After implicit conversion
    
    -- Division
    ("/", _, _) -> return (T.pack "f32.div") -- Always float division
    
    -- Integer division
    ("div", IntType, IntType) -> return (T.pack "i32.div_s")
    
    -- Comparison
    ("<", IntType, IntType) -> return (T.pack "i32.lt_s")
    ("<", FloatType, FloatType) -> return (T.pack "f32.lt")
    (">", IntType, IntType) -> return (T.pack "i32.gt_s")
    (">", FloatType, FloatType) -> return (T.pack "f32.gt")
    ("==", IntType, IntType) -> return (T.pack "i32.eq")
    ("==", FloatType, FloatType) -> return (T.pack "f32.eq")
    
    -- Logical operators
    ("&&", BoolType, BoolType) -> return (T.pack "i32.and")
    ("||", BoolType, BoolType) -> return (T.pack "i32.or")
    
    -- Not supported
    _ -> throwError $ CodeGenErr $ UnsupportedOperation $ 
         op `T.append` (T.pack " with types ") `T.append` 
         T.pack (show leftType) `T.append` (T.pack " and ") `T.append` 
         T.pack (show rightType)

-- | Compile a unary operation
compileUnaryOp :: Text -> BPROGExpr -> CompilerState -> Compiler (Text, CompilerState)
compileUnaryOp op operand state = do
  -- First compile the operand
  (operandCode, newState) <- compileExpr operand state
  
  -- Infer the type of the operand
  operandType <- inferType operand state
  
  -- Generate the appropriate WebAssembly operation based on the type
  opCode <- getWasmUnaryOp op operandType
  
  let watCode = operandCode `T.append` 
                T.pack ("    ;; " ++ T.unpack op ++ " operation\n" ++ 
                        "    " ++ T.unpack opCode ++ "\n")
  
  return (watCode, newState)

-- | Get the WebAssembly unary operation based on BPROG operation and operand type
getWasmUnaryOp :: Text -> BPROGType -> Compiler Text
getWasmUnaryOp op operandType = 
  let opStr = T.unpack op in
  case opStr of
    -- Logical NOT
    "not" -> case operandType of
      BoolType -> return (T.pack "i32.eqz") -- Boolean NOT
      IntType -> return (T.pack "i32.const -1\ni32.mul") -- Numeric negation for integers
      FloatType -> return (T.pack "f32.const -1\nf32.mul") -- Numeric negation for floats
      _ -> throwError $ CodeGenErr $ UnsupportedOperation $ 
           op `T.append` (T.pack " with type ") `T.append` T.pack (show operandType)
    
    -- String parsing operations
    "parseInteger" -> case operandType of
      StringType -> return (T.pack "call $parse_integer")
      _ -> throwError $ CodeGenErr $ UnsupportedOperation $ 
           op `T.append` (T.pack " with type ") `T.append` T.pack (show operandType)
    
    "parseFloat" -> case operandType of
      StringType -> return (T.pack "call $parse_float")
      _ -> throwError $ CodeGenErr $ UnsupportedOperation $ 
           op `T.append` (T.pack " with type ") `T.append` T.pack (show operandType)
    
    "words" -> case operandType of
      StringType -> return (T.pack "call $split_words")
      _ -> throwError $ CodeGenErr $ UnsupportedOperation $ 
           op `T.append` (T.pack " with type ") `T.append` T.pack (show operandType)
    
    -- Not supported
    _ -> throwError $ CodeGenErr $ UnsupportedOperation $ 
         op `T.append` (T.pack " with type ") `T.append` T.pack (show operandType)

-- | Compile a stack operation
compileStackOp :: Text -> CompilerState -> Compiler (Text, CompilerState)
compileStackOp op state = 
  let opStr = T.unpack op in
  case opStr of
    -- Duplicate top of stack
    "dup" -> do
      let watCode = T.pack "    ;; Duplicate top of stack\n" `T.append`
                  T.pack "    local.tee $temp\n" `T.append`
                  T.pack "    local.get $temp\n"
      return (watCode, state)
    
    -- Swap top two elements
    "swap" -> do
      let watCode = T.pack "    ;; Swap top two stack values\n" `T.append`
                  T.pack "    local.set $temp1\n" `T.append`
                  T.pack "    local.set $temp2\n" `T.append`
                  T.pack "    local.get $temp1\n" `T.append`
                  T.pack "    local.get $temp2\n"
      return (watCode, state)
    
    -- Pop top element
    "pop" -> do
      let watCode = T.pack "    ;; Pop top of stack\n" `T.append`
                  T.pack "    drop\n"
      return (watCode, state)
    
    -- Not supported
    _ -> throwError $ CodeGenErr $ UnsupportedOperation op

-- | Infer the type of an expression
inferType :: BPROGExpr -> CompilerState -> Compiler BPROGType
inferType expr state = case expr of
  IntLit _ -> return IntType
  FloatLit _ -> return FloatType
  BoolLit _ -> return BoolType
  StringLit _ -> return StringType
  ListLit items -> do
    -- Infer the type of list elements
    if null items
      then return $ ListType UnknownType -- Empty list has unknown element type
      else do
        itemType <- inferType (head items) state
        return $ ListType itemType
  
  SymbolRef name -> inferSymbolType name state
  
  BinaryOp op left right -> inferBinaryOpType op left right state
  
  UnaryOp op operand -> inferUnaryOpType op operand state
  
  -- Other expression types
  _ -> return UnknownType

-- | Infer the type of a symbol
inferSymbolType :: Text -> CompilerState -> Compiler BPROGType
inferSymbolType name state =
  case lookup name (symbolTable state) of
    Just (typ, _) -> return typ
    Nothing -> return UnknownType

-- | Infer the type of a binary operation
inferBinaryOpType :: Text -> BPROGExpr -> BPROGExpr -> CompilerState -> Compiler BPROGType
inferBinaryOpType op left right state = do
  leftType <- inferType left state
  rightType <- inferType right state
  
  case T.unpack op of
    -- Arithmetic operations
    "+" -> case (leftType, rightType) of
      (IntType, IntType) -> return IntType
      (FloatType, _) -> return FloatType
      (_, FloatType) -> return FloatType
      _ -> return UnknownType
    
    "-" -> case (leftType, rightType) of
      (IntType, IntType) -> return IntType
      (FloatType, _) -> return FloatType
      (_, FloatType) -> return FloatType
      _ -> return UnknownType
    
    "*" -> case (leftType, rightType) of
      (IntType, IntType) -> return IntType
      (FloatType, _) -> return FloatType
      (_, FloatType) -> return FloatType
      _ -> return UnknownType
    
    "/" -> return FloatType -- Always produces float
    
    "div" -> return IntType -- Always produces int
    
    -- Comparison operations
    "<" -> return BoolType
    ">" -> return BoolType
    "==" -> return BoolType
    
    -- Logical operations
    "&&" -> return BoolType
    "||" -> return BoolType
    
    -- Default
    _ -> return UnknownType

-- | Infer the type of a unary operation
inferUnaryOpType :: Text -> BPROGExpr -> CompilerState -> Compiler BPROGType
inferUnaryOpType op operand state = do
  operandType <- inferType operand state
  
  case T.unpack op of
    "not" -> case operandType of
      BoolType -> return BoolType
      IntType -> return IntType
      FloatType -> return FloatType
      _ -> return UnknownType
    
    "parseInteger" -> return IntType
    "parseFloat" -> return FloatType
    "words" -> return $ ListType StringType
    
    -- Default
    _ -> return UnknownType

-- | Type check an expression
typeCheck :: BPROGExpr -> CompilerState -> Compiler ()
typeCheck expr state = case expr of
  -- Literals don't need type checking
  IntLit _ -> return ()
  FloatLit _ -> return ()
  BoolLit _ -> return ()
  StringLit _ -> return ()
  
  -- Check list items
  ListLit items -> mapM_ (`typeCheck` state) items
  
  -- Check that symbol is defined
  SymbolRef name ->
    case lookup name (symbolTable state) of
      Just _ -> return ()
      Nothing -> throwError $ TypeCheckErr $ UndefinedSymbol name
  
  -- Check binary operations
  BinaryOp op left right -> do
    typeCheck left state
    typeCheck right state
    leftType <- inferType left state
    rightType <- inferType right state
    checkBinaryOpTypes op leftType rightType
  
  -- Check unary operations
  UnaryOp op operand -> do
    typeCheck operand state
    operandType <- inferType operand state
    checkUnaryOpType op operandType
  
  -- Other expressions
  _ -> return ()

-- | Check types for binary operations
checkBinaryOpTypes :: Text -> BPROGType -> BPROGType -> Compiler ()
checkBinaryOpTypes op leftType rightType = case T.unpack op of
  -- Arithmetic operations
  "+" -> checkArithmeticTypes leftType rightType
  "-" -> checkArithmeticTypes leftType rightType
  "*" -> checkArithmeticTypes leftType rightType
  "/" -> checkArithmeticTypes leftType rightType
  "div" -> checkIntegerTypes leftType rightType
  
  -- Comparison operations
  "<" -> checkComparisonTypes leftType rightType
  ">" -> checkComparisonTypes leftType rightType
  "==" -> return () -- Any types can be compared for equality
  
  -- Logical operations
  "&&" -> checkBooleanTypes leftType rightType
  "||" -> checkBooleanTypes leftType rightType
  
  -- Default
  _ -> return ()
  
  where
    checkArithmeticTypes :: BPROGType -> BPROGType -> Compiler ()
    checkArithmeticTypes leftType rightType =
      case (leftType, rightType) of
        (IntType, IntType) -> return ()
        (FloatType, FloatType) -> return ()
        (IntType, FloatType) -> return ()
        (FloatType, IntType) -> return ()
        _ -> throwError $ TypeCheckErr $ InvalidOperandType op $
             T.pack (show leftType) `T.append` (T.pack " and ") `T.append`
             T.pack (show rightType)
    
    checkIntegerTypes :: BPROGType -> BPROGType -> Compiler ()
    checkIntegerTypes leftType rightType =
      case (leftType, rightType) of
        (IntType, IntType) -> return ()
        _ -> throwError $ TypeCheckErr $ InvalidOperandType op $
             T.pack (show leftType) `T.append` (T.pack " and ") `T.append`
             T.pack (show rightType)
    
    checkComparisonTypes :: BPROGType -> BPROGType -> Compiler ()
    checkComparisonTypes leftType rightType =
      case (leftType, rightType) of
        (IntType, IntType) -> return ()
        (FloatType, FloatType) -> return ()
        (IntType, FloatType) -> return ()
        (FloatType, IntType) -> return ()
        _ -> throwError $ TypeCheckErr $ InvalidOperandType op $
             T.pack (show leftType) `T.append` (T.pack " and ") `T.append`
             T.pack (show rightType)
    
    checkBooleanTypes :: BPROGType -> BPROGType -> Compiler ()
    checkBooleanTypes leftType rightType =
      case (leftType, rightType) of
        (BoolType, BoolType) -> return ()
        _ -> throwError $ TypeCheckErr $ InvalidOperandType op $
             T.pack (show leftType) `T.append` (T.pack " and ") `T.append`
             T.pack (show rightType)

-- | Check types for unary operations
checkUnaryOpType :: Text -> BPROGType -> Compiler ()
checkUnaryOpType op operandType = case T.unpack op of
  "not" -> case operandType of
    BoolType -> return ()
    IntType -> return ()
    FloatType -> return ()
    _ -> throwError $ TypeCheckErr $ InvalidOperandType op $ 
         T.pack (show operandType)
  
  "parseInteger" -> case operandType of
    StringType -> return ()
    _ -> throwError $ TypeCheckErr $ InvalidOperandType op $ 
         T.pack (show operandType)
  
  "parseFloat" -> case operandType of
    StringType -> return ()
    _ -> throwError $ TypeCheckErr $ InvalidOperandType op $ 
         T.pack (show operandType)
  
  "words" -> case operandType of
    StringType -> return ()
    _ -> throwError $ TypeCheckErr $ InvalidOperandType op $ 
         T.pack (show operandType)
  
  -- Default
  _ -> return ()

-- | Assign memory locations to global variables and constants
assignMemoryLocations :: CompilerState -> Compiler CompilerState
assignMemoryLocations state = do
  -- This is a placeholder for the actual implementation
  -- In a real compiler, this would assign memory addresses to variables
  return state