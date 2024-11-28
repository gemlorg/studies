{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}

module Typechecker.Utils where

import           Common.Exception
import           Common.RTypes
import           Common.GrammarUtils
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List             (nub, sort)
import           Grammar.Abs
import           Typechecker.Environment
import           Typechecker.Monad

import           Prelude
import Control.Exception (assert, throw)

getArgumentsWithTypes :: [Arg] -> [RVariable]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> RVariable
getArgumentWithType (Arg _ argType name)    = (name, fromType argType)

validateFunctionArguments :: [Arg] -> Bool
validateFunctionArguments arguments =
  numberOfArguments == numberOfUniqueArguments
  where
    argumentsNames = map getArgumentName arguments
    numberOfArguments = length argumentsNames
    numberOfUniqueArguments = length $ nub argumentsNames

getArgumentName :: Arg -> Ident
getArgumentName (Arg _ _ name)    = name

isFunctionType :: Type -> Bool
isFunctionType Fun {} = True
isFunctionType _      = False


assertTypeG :: BNFC'Position -> RawType -> RawType -> EmptyTypegetterMonad
assertTypeG pos t1 t2 =
  if t1 == t2
    then pure ()
    else throwError $ Exception (InvalidTypeException t2 t1) pos

assertNotFunctionG :: BNFC'Position -> RawType -> EmptyTypegetterMonad
assertNotFunctionG pos t =
  if isFunctionRawType t
    then throwError $ Exception (InvalidTypeException t RTVoid) pos
    else pure ()

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

assertDeclType:: BNFC'Position -> Type -> TypecheckerMonad
assertDeclType pos t = case fromType t of
  RTVoid -> throwError $ Exception (InvalidDeclException) pos
  RTFun _ _ -> throwError $ Exception (InvalidDeclException) pos
  _         -> pure ()

assertMain :: Env -> TypecheckerMonad
assertMain env = 
  let (name, typ) = mainSignature
  in case getType env name of
    Just t  -> if t == typ then 
      pure () else 
        throwError $ Exception (InvalidMainTypeException t) (NoPos)
    Nothing -> throwError $ Exception (NoMainException) (NoPos)

assertIntRange :: BNFC'Position -> Integer -> TypegetterMonad
assertIntRange pos i = 
  if i >= minBound32 && i <= maxBound32
    then pure RTInt
    else throwError $ Exception (InvalidIntRangeException i) pos
  where 
    minBound32 = 2^(31 :: Integer) * (-1)
    maxBound32 = 2^(31 :: Integer) - 1

isFunctionRawType :: RawType -> Bool
isFunctionRawType RTFun {} = True
isFunctionRawType _        = False

assertVarNotExists :: BNFC'Position -> Ident -> TypecheckerMonad 
assertVarNotExists pos name = do
  env <- get
  case getType env name of
    Just _  -> throwError $ Exception (SymbolAlreadyDefinedException name) pos
    Nothing -> pure ()

assertUniueArgs :: BNFC'Position -> [Arg] -> TypecheckerMonad
assertUniueArgs pos args = 
  if areUniqueArgs args
    then pure ()
    else throwError $ Exception (ArgDuplicateException args) pos

assertVarType :: BNFC'Position -> Ident -> RawType  -> TypecheckerMonad
assertVarType pos name t = do
  env <- get
  case getType env name of
    Just t' -> assertTypeC pos t t'
    Nothing -> throwError $ Exception (UndefinedSymbolException name) pos

assertNotVoidArgs :: BNFC'Position -> [Arg] -> TypecheckerMonad 
assertNotVoidArgs _ args = 
  mapM_ (assertNotVoid ) args

assertNotVoid :: Arg -> TypecheckerMonad 
assertNotVoid (Arg pos t name) = 
  if fromType t == RTVoid
    then throwError $ Exception (VoidArgumentException name) pos
    else pure ()
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

isTrue :: Expr -> Maybe Bool
isTrue (ELitTrue _) = Just True
isTrue (ELitFalse _) = Just False
isTrue _            = Nothing