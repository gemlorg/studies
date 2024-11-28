module Common.RTypes where

import           Grammar.Abs


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


predifinedFunctions :: [RVariable]
predifinedFunctions =
  [ printIntSignature
  , printStringSignature
  , errorSignature
  , readIntSignature
  , readStringSignature
  ]

data RawType
  = RTInt
  | RTString
  | RTBool
  | RTVoid
  | RTFun RawType [RawType] 
  deriving (Ord)

instance Eq RawType where
  RTInt == RTInt = True
  RTString == RTString = True
  RTBool == RTBool = True
  RTVoid == RTVoid = True
  (RTFun args1 returnType1) == (RTFun args2 returnType2) =
    args1 == args2 && returnType1 == returnType2
  _ == _ = False

instance Show RawType where
  show RTInt = "int"
  show RTString = "string"
  show RTBool = "boolean"
  show RTVoid = "void"
  show (RTFun argsTypes returnType) =
    concat [ show returnType, "(", show argsTypes, ")"]

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

fromFunction ::  Type -> [Arg]   -> RawType
fromFunction returnType arguments = RTFun rawReturnType rawArgumentsTypes
  where
    rawArgumentsTypes = fromArgs arguments
    rawReturnType = fromType returnType

fromArgs :: [Arg] -> [RawType]
fromArgs = map fromArg

fromArg :: Arg -> RawType
fromArg (Arg _ argType _)    = fromType argType
