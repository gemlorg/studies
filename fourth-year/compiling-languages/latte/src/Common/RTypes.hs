module Common.RTypes where

import           Grammar.Abs
import LLVM.AST (Name)


type RVariable = (Ident, RawType)

printIntSignature :: RVariable
printIntSignature = (Ident "printInt", RTFun RTVoid [RTInt])

printStringSignature :: RVariable
printStringSignature = (Ident "printString", RTFun RTVoid [RTString])

errorSignature :: RVariable
errorSignature = (Ident "error", RTFun RTVoid [])

readIntSignature :: RVariable
readIntSignature = (Ident "readInt", RTFun RTInt [])

readStringSignature :: RVariable
readStringSignature = (Ident "readString", RTFun RTString [])

mainSignature :: RVariable
mainSignature = (Ident "main", RTFun RTInt [])

concatStringsSignature :: RVariable
concatStringsSignature = (Ident "_concatStrings", RTFun RTString [RTString, RTString])

compareStringsSignature :: RVariable
compareStringsSignature = (Ident "_compareStrings", RTFun RTInt [RTString, RTString])
mallocSignature :: RVariable
mallocSignature = (Ident "_malloc", RTFun RTString [RTInt])

countSignature :: RVariable
countSignature = (Ident "_count_arr_length", RTFun RTInt [RTString, RTInt])

predifinedFunctions :: [RVariable]
predifinedFunctions =
  [ printIntSignature
  , printStringSignature
  , errorSignature
  , readIntSignature
  , readStringSignature

  ]
predifinedFunctionsInternal :: [RVariable]
predifinedFunctionsInternal = predifinedFunctions ++ [concatStringsSignature, compareStringsSignature , mallocSignature]

data RawType
  = RTInt
  | RTString
  | RTBool
  | RTVoid
  | RTFun RawType [RawType] 
  | RTClass Ident
  | RTArr RawType 
  deriving (Ord)

data Value = VInt Integer
            | VFalse
            | VTrue
            | VReference Name RawType

  deriving (Show, Eq, Ord)

instance Eq RawType where
  RTInt == RTInt = True
  RTString == RTString = True
  RTBool == RTBool = True
  RTVoid == RTVoid = True
  (RTFun args1 returnType1) == (RTFun args2 returnType2) =
    args1 == args2 && returnType1 == returnType2
  (RTClass ident1) == (RTClass ident2) = ident1 == ident2
  (RTArr t1) == (RTArr t2 ) = t1 == t2
  _ == _ = False

instance Show RawType where
  show RTInt = "int"
  show RTString = "string"
  show RTBool = "boolean"
  show RTVoid = "void"
  show (RTFun argsTypes returnType) =
    concat [ show returnType, "(", show argsTypes, ")"]
  show (RTClass ident) = show ident
  show (RTArr t ) = show t ++ "[]"

fromType :: Type -> RawType
fromType (Int _) = RTInt
fromType (Str _) = RTString
fromType (Bool _) = RTBool
fromType (Void _) = RTVoid
fromType (Fun _ returnType argumentsTypes) =
  (RTFun rawReturnType rawArgumentsTypes)
  where
    rawArgumentsTypes = map fromType argumentsTypes
    rawReturnType = fromType returnType
fromType (Class _ ident) = RTClass ident
fromType (Array _ t) = RTArr (fromType t) 




fromFunction ::  Type -> [Arg]   -> RawType
fromFunction returnType arguments = RTFun rawReturnType rawArgumentsTypes
  where
    rawArgumentsTypes = fromArgs arguments
    rawReturnType = fromType returnType

fromArgs :: [Arg] -> [RawType]
fromArgs = map fromArg

fromArg :: Arg -> RawType
fromArg (Arg _ argType _)    = fromType argType
