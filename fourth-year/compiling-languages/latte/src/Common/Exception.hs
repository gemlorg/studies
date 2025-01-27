{-# LANGUAGE FlexibleInstances #-}

module Common.Exception where

import           Grammar.Abs

import           Common.GrammarUtils
import           Common.RTypes


data TracedException e =
  Exception e BNFC'Position

instance Show e => Show (TracedException e) where

  show (Exception e pos) 
    | pos == NoPos = concat ["error: ", show e]
    | otherwise = concat ["error: ", show e, " at ", showPos pos]

type CompileException = TracedException CompileException'

data CompileException'
  = DivideByZeroException
  | InvalidStepException

type StaticException = TracedException StaticException'

data StaticException'
  = UndefinedSymbolException Ident
  | InvalidTypeException RawType RawType
  | ExpectedFunctionException RawType
  | InvalidFunctionArgumentsTypesException [RawType] [RawType]
  | ArgDuplicateException [Arg]
  | InvalidReturnTypeException RawType
  | ReturnOutOfScopeException
  | NoReturnStatementException
  | NoMainException
  | InvalidDeclException
  | SymbolAlreadyDefinedException Ident
  | ExpectedVariableException RawType
  | InvalidIntRangeException Integer
  | InvalidMainTypeException RawType
  | VoidArgumentException Ident
  | VoidExprException 
  | NoBlockDeclException
  | ShowException String
  | NoSuchClassException Ident
  | ExpectedClassException RawType
  | NotArrayException RawType
  

instance Show CompileException' where
  show DivideByZeroException = "division by zero"
  show InvalidStepException  = "step has to be non-zero"

instance Show StaticException' where
  show (UndefinedSymbolException name) = "undefined symbol: " ++ showIdent name
  show (InvalidTypeException expected actual) =
    "invalid type. expected " ++ show expected ++ ", got " ++ show actual
  show (ExpectedFunctionException actual) =
    "expected function, got " ++ show actual
  show (InvalidFunctionArgumentsTypesException expected actual) =
    "invalid function arguments. expected "
      ++ show expected
      ++ ", got "
      ++ show actual
  show (ArgDuplicateException arguments) =
    "function arguments names duplication: " ++ show (map showArg arguments)
  show (InvalidReturnTypeException expected) =
    "invalid return type. expected " ++ show expected
  show ReturnOutOfScopeException = "return statement out of scope"
  show NoReturnStatementException = "a block has no return statement"
  show NoMainException = "no main function"
  show InvalidDeclException = "invalid declaration type"
  show (SymbolAlreadyDefinedException name) =
    "symbol already defined: " ++ showIdent name
  show (ExpectedVariableException typ) = "the operator is not defined for type: " ++ show typ
  show (InvalidIntRangeException i) = "integer out of range: " ++ show i
  show (InvalidMainTypeException typ) = "invalid main function type: " ++ show typ
  show (VoidArgumentException name) = "void argument: " ++ showIdent name 
  show (ShowException s) = s
  show VoidExprException = "void function shoudln't return an expression"
  show NoBlockDeclException = "declaration outside of a block"
  show (NoSuchClassException name) = "no such class: " ++ showIdent name
  show (ExpectedClassException typ) = "expected class, got: " ++ show typ
  show (NotArrayException typ) = "expected array, got: " ++ show typ
