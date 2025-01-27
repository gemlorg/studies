module Typechecker.Monad where

import           Common.Exception
import           Common.RTypes
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Prelude
import           Typechecker.Environment
import Grammar.Abs
import qualified Data.Map as Map
import Lens.Micro

type TypecheckerMonad = TypecheckerMonad' ()

type TypecheckerMonad' a = StateT Env (Except StaticException)  a

type TypegetterMonad = TypegetterMonad' RawType

type EmptyTypegetterMonad = TypegetterMonad' ()

type TypegetterMonad' a = ReaderT Env (Except StaticException) a

class Typechecker a where
  checkTypeM :: Maybe RawType -> a -> TypecheckerMonad

class Typegetter a where
  getTypeM :: a -> TypegetterMonad


getFields :: RawType -> BNFC'Position -> TypegetterMonad' [(Ident, RawType)]
getFields (RTClass name) pos = do
  env <- ask
  case Map.lookup name (env ^. classFields) of
    Just members -> pure members
    Nothing -> throwError $ Exception (NoSuchClassException name) pos

getFields typ pos = throwError $ Exception (ExpectedClassException typ) pos

getMethods :: RawType -> BNFC'Position -> TypegetterMonad' [(Ident, RawType)]
getMethods (RTClass name) pos = do
  env <- ask
  case Map.lookup name (env ^. classMethods) of
    Just members -> pure members
    Nothing -> throwError $ Exception (NoSuchClassException name) pos

getMethods typ pos = throwError $ Exception (ExpectedClassException typ) pos
