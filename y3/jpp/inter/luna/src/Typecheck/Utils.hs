{-# LANGUAGE TupleSections #-}

module Typecheck.Utils where

import           Grammar.Abs
import           Data.List

import           Common.Exception
import           Typecheck.Environment
import           Typecheck.Exception
import           Typecheck.Monad

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Prelude


import           Data.List
import           Grammar.Abs
import           Typecheck.Environment
import Control.Exception (throw)


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

-- validateInitNames :: [Init] -> Bool
-- validateInitNames inits = numberOfInits == numberOfUniqueInits
--   where
--     names = map getInitName inits
--     numberOfInits = length names
--     numberOfUniqueInits = length $ nub names

-- getInitName :: Init -> Ident
-- getInitName (IFnDef _ name _ _ _) = name
-- getInitName (IInit _ name _ _)    = name

getArgumentName :: Arg -> Ident
getArgumentName (AArg _ name _)    = name
getArgumentName (AArgVar _ name _) = name


isFunctionType :: Type -> Bool
isFunctionType Fun {} = True
isFunctionType _       = False



-- my 
assertType :: BNFC'Position -> RawType -> RawType -> EmptyTypegetterM
assertType pos t1 t2 = if t1 == t2 then pure () else throwError $ InvalidTypeException pos t1 t2

getVarType :: BNFC'Position -> Ident -> TypegetterM
getVarType pos name = do
  env <- ask
  case getType env name of
    Just t -> pure t
    Nothing -> throwError $ UndefinedSymbolException pos name