{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Typecheck.Environment where 



import qualified Data.Map               as M
import           Prelude
import           Grammar.Abs

data Env = Env
  { types                         :: M.Map Ident RawType
  , hasReturnStatementOccuredFlag :: Bool
  }

emptyEnv :: Env
emptyEnv = Env
  { types = M.fromList [printSignature]
  , hasReturnStatementOccuredFlag = False
  }

printSignature :: (Ident, RawType)
printSignature = (Ident "print", RTFun [RTAny] RTNil)

data RawType
    = RTInt
    | RTString
    | RTBool
    | RTNil
    | RTAny
    | RTFun [RawType] RawType

instance Eq RawType where
  RTInt == RTInt = True
  RTString == RTString = True
  RTBool == RTBool = True
  RTNil == RTNil = True
  (RTFun args1 returnType1) == (RTFun args2 returnType2) = args1 == args2 && returnType1 == returnType2
  RTAny == RTAny = True
  _ == _ = False




instance Show RawType where

  show RTInt = "Int"

  show RTString = "String"

  show RTBool = "Bool"

  show RTNil = "Nil"

  show (RTFun argsTypes returnType) = concat [
    "fn(", show argsTypes, ")", " -> ", show returnType
    ]
  show RTAny = "Any"
  


fromType :: Type -> RawType
fromType (Int _) = RTInt
fromType (Str _) = RTString
fromType (Bool _) = RTBool
fromType (Nil _) = RTNil
fromType (Fun _ argumentsTypes returnType) = (RTFun rawArgumentsTypes rawReturnType) where
    rawArgumentsTypes = map fromType argumentsTypes
    rawReturnType = fromType returnType
fromType _ = RTNil 

    

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

updateTypes :: Env -> [(Ident, RawType)] -> Env
updateTypes = foldl updateTupleType

updateTupleType :: Env -> (Ident, RawType) -> Env
updateTupleType env (name, newType) = updateType env name newType

updateType :: Env -> Ident -> RawType -> Env
updateType Env{..} name newType = Env {types=M.insert name newType types, hasReturnStatementOccuredFlag=hasReturnStatementOccuredFlag}


getType :: Env -> Ident -> Maybe RawType
getType Env{..} name = M.lookup name types


returnStatementOccured :: Env -> Env
returnStatementOccured Env{..} = Env {types=types, hasReturnStatementOccuredFlag=True}

hasReturnStatementOccured :: Env -> Bool
hasReturnStatementOccured Env{..} = hasReturnStatementOccuredFlag
