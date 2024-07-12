{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}

module Typecheck.Utils where

import           Common.Exception
import           Common.RTypes
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List             (nub, sort)
import           Grammar.Abs
import           Typecheck.Environment
import           Typecheck.Monad

import           Prelude

getArgumentsWithTypes :: [Arg] -> [(Ident, RawType)]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> (Ident, RawType)
getArgumentWithType (AArg _ name argType)    = (name, fromType argType)
getArgumentWithType (AArgVar _ name argType) = (name, fromType argType)

validateFunctionArguments :: [Arg] -> Bool
validateFunctionArguments arguments =
  numberOfArguments == numberOfUniqueArguments
  where
    argumentsNames = map getArgumentName arguments
    numberOfArguments = length argumentsNames
    numberOfUniqueArguments = length $ nub argumentsNames

getArgumentName :: Arg -> Ident
getArgumentName (AArg _ name _)    = name
getArgumentName (AArgVar _ name _) = name

isFunctionType :: Type -> Bool
isFunctionType Fun {} = True
isFunctionType _      = False

-- my
assertTypeG :: BNFC'Position -> RawType -> RawType -> EmptyTypegetterMonad
assertTypeG pos t1 t2 =
  if t1 == t2
    then pure ()
    else throwError $ Exception (InvalidTypeException t2 t1) pos

assertTypeC :: BNFC'Position -> RawType -> RawType -> TypecheckerMonad
assertTypeC pos t1 t2 =
  if t1 == t2
    then pure ()
    else throwError $ Exception (InvalidTypeException t2 t1) pos

assertVarExistsG :: BNFC'Position -> Ident -> TypegetterMonad
assertVarExistsG pos name = do
  env <- ask
  case getType env name of
    Just t  -> pure t
    Nothing -> throwError $ Exception (UndefinedSymbolException name) pos

getVarType :: BNFC'Position -> Ident -> Env -> Either StaticException RawType
getVarType position name env =
  case getType env name of
    Just t  -> pure t
    Nothing -> throwError $ Exception (UndefinedSymbolException name) position

areUniqueArgs :: [Arg] -> Bool
areUniqueArgs arguments = do
  let sortedNames = sort $ map getArgumentName arguments
  all (uncurry (/=)) $ zip sortedNames (tail sortedNames)

runExpr :: Typegetter Expr => Env -> Expr -> Either StaticException RawType
runExpr env expr = runExcept $ runReaderT (getTypeM expr) env
