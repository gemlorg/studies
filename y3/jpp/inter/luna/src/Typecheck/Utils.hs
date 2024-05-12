{-# LANGUAGE TupleSections #-}

module Typecheck.Utils where

import           Grammar.Abs
import Data.List ( zip, map, length, all, tail, nub, sort )
import           Typecheck.Environment
import           Typecheck.Exception
import Typecheck.Monad
    ( EmptyTypegetterM, TypegetterM, TypecheckerM )
import           Control.Monad.Except
import           Control.Monad.Reader

import           Prelude



getArgumentsWithTypes :: [Arg] -> [(Ident, RawType)]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> (Ident, RawType)
getArgumentWithType (AArg _ name argType)    = (name, fromType argType)
getArgumentWithType (AArgVar _ name argType) = (name, fromType argType)


validateFunctionArguments :: [Arg] -> Bool
validateFunctionArguments arguments = numberOfArguments == numberOfUniqueArguments
  where
    argumentsNames = map getArgumentName arguments
    numberOfArguments = length argumentsNames
    numberOfUniqueArguments = length $ nub argumentsNames


getArgumentName :: Arg -> Ident
getArgumentName (AArg _ name _)    = name
getArgumentName (AArgVar _ name _) = name


isFunctionType :: Type -> Bool
isFunctionType Fun {} = True
isFunctionType _       = False



-- my 
assertTypeG :: BNFC'Position -> RawType -> RawType -> EmptyTypegetterM
assertTypeG pos t1 t2 = if t1 == t2 then pure () else throwError $ InvalidTypeException pos t2 t1

assertTypeC :: BNFC'Position -> RawType -> RawType -> TypecheckerM
assertTypeC pos t1 t2 = if t1 == t2 then pure () else throwError $ InvalidTypeException pos t2 t1



assertVarExistsG :: BNFC'Position -> Ident  -> TypegetterM
assertVarExistsG pos name = do
  env <- ask
  case getType env name of
    Just t -> pure t
    Nothing -> throwError $ UndefinedSymbolException pos name

getVarType :: BNFC'Position -> Ident -> Env -> Either TypecheckingException RawType
getVarType position name env = case getType env name of
  Just t -> pure t
  Nothing -> throwError $ UndefinedSymbolException position name


areUniqueArgs:: [Arg] -> Bool
areUniqueArgs arguments =  do 
  let sortedNames = sort $ map getArgumentName arguments
  all (uncurry (/=)) $ zip sortedNames (tail sortedNames)

