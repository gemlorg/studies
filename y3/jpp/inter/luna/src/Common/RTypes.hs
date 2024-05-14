module Common.RTypes where

import           Grammar.Abs

printSignature :: (Ident, RawType)
printSignature = (Ident "print", RTFun [RTAny] RTNil)

data RawType
  = RTInt
  | RTString
  | RTBool
  | RTNil
  | RTAny
  | RTFun [RawType] RawType
  deriving (Ord)

instance Eq RawType where
  RTInt == RTInt = True
  RTString == RTString = True
  RTBool == RTBool = True
  RTNil == RTNil = True
  (RTFun args1 returnType1) == (RTFun args2 returnType2) =
    args1 == args2 && returnType1 == returnType2
  RTAny == RTAny = True
  _ == _ = False

instance Show RawType where
  show RTInt = "Int"
  show RTString = "String"
  show RTBool = "Bool"
  show RTNil = "Nil"
  show (RTFun argsTypes returnType) =
    concat ["fn(", show argsTypes, ")", " -> ", show returnType]
  show RTAny = "Any"

fromType :: Type -> RawType
fromType (Int _) = RTInt
fromType (Str _) = RTString
fromType (Bool _) = RTBool
fromType (Nil _) = RTNil
fromType (Fun _ argumentsTypes returnType) =
  (RTFun rawArgumentsTypes rawReturnType)
  where
    rawArgumentsTypes = map fromType argumentsTypes
    rawReturnType = fromType returnType

fromFunction :: [Arg] -> Type -> RawType
fromFunction arguments returnType = RTFun rawArgumentsTypes rawReturnType
  where
    rawArgumentsTypes = fromArgs arguments
    rawReturnType = fromType returnType

fromArgs :: [Arg] -> [RawType]
fromArgs = map fromArg

fromArg :: Arg -> RawType
fromArg (AArg _ _ argType)    = fromType argType
fromArg (AArgVar _ _ argType) = fromType argType
