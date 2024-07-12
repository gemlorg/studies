module Typecheck.Monad where

import           Common.Exception
import           Common.RTypes
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Prelude
import           Typecheck.Environment

type TypecheckerMonad = TypecheckerMonad' ()

type TypecheckerMonad' a = StateT Env (Except StaticException) a

type TypegetterMonad = TypegetterMonad' RawType

type EmptyTypegetterMonad = TypegetterMonad' ()

type TypegetterMonad' a = ReaderT Env (Except StaticException) a

class Typechecker a where
  checkTypeM :: Maybe RawType -> a -> TypecheckerMonad

class Typegetter a where
  getTypeM :: a -> TypegetterMonad
