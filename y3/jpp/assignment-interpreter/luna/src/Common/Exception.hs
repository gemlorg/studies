{-# LANGUAGE FlexibleInstances #-}

module Common.Exception where

import           Grammar.Abs

import           Common.GrammarUtils
import           Common.RTypes

data TracedException e =
  Exception e BNFC'Position

instance Show e => Show (TracedException e) where
  show (Exception e pos) = concat ["error: ", show e, " at ", showPos pos]

type RuntimeException = TracedException RuntimeException'

data RuntimeException'
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

instance Show RuntimeException' where
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
