{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Typecheck.Exception where

import           Grammar.Abs

import           Common.Exception


import Typecheck.Environment


type TypecheckingException = TypecheckingException' BNFC'Position

data TypecheckingException' a
  = UndefinedSymbolException a Ident
  | InvalidTypeException a RawType RawType
  | ExpectedFunctionException a RawType
  | InvalidFunctionArgumentsTypesException a [RawType] [RawType]
  | FunctionArgumentsNameDuplicationException a [Arg]
  | InvalidReturnTypeException a RawType
  | ReturnOutOfScopeException a
  | NoReturnStatementException a
  | NamesDuplicationException a


instance Show TypecheckingException where

  show (UndefinedSymbolException position name) = concat [
    "ERROR: Undefined symbol: ", showIdent name, ", at ", showPos position
    ]

  show (InvalidTypeException position expectedType actualType) = concat [
    "ERROR: Invalid type! Expected: ", show expectedType, ", got: ", show actualType, ", at ", showPos position
    ]

  show (ExpectedFunctionException position actualType) = concat [
    "ERROR: Expected function! Got: ", show actualType, ", at ", showPos position
    ]

  show (InvalidFunctionArgumentsTypesException position expectedTypes actualTypes) = concat [
    "ERROR: Invalid function arguments! Expected: ", show expectedTypes, ", got: ", show actualTypes, ", at ", showPos position
    ]

  show (FunctionArgumentsNameDuplicationException position arguments) = concat [
    "ERROR: Function arguments names duplication: ", show (map showArg arguments), ", at ", showPos position
    ]

  show (InvalidReturnTypeException position expectedType) = concat [
    "ERROR: Invalid function return type! Expected: ", show expectedType, ", at ", showPos position
    ]

  show (ReturnOutOfScopeException position) =
    "ERROR: Return statement out of scope, at " ++ showPos position

  show (NoReturnStatementException position) =
    "ERROR: Block has no return statement at: " ++ showPos position

  show (NamesDuplicationException position) =
    "ERROR: Names duplication at: " ++showPos position


showIdent :: Ident -> String
showIdent (Ident name) = name

showPos :: BNFC'Position -> String
showPos (Just (line, column)) = concat ["line ", show line, ", column ", show column]
showPos _ = "unknow"


showArg :: Arg -> String
showArg (AArg _ ident typ) = concat [showIdent ident, ": "]
showArg (AArgVar _ ident typ) = concat ["var ", showIdent ident, ": "]

showType :: Type -> String
showType (Int _) = "Int"
showType (Str _) = "String"
showType (Bool _) = "Bool"
showType (Nil _) = "Void"
showType (Fun _ args returnType) = concat ["(", show args, ")", " -> ", show returnType]
