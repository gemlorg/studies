module Eval.Monad where

import           Eval.Environment
import           Common.Exception

import           Control.Monad.Except
import           Control.Monad.State


type EvalMonad = EvalMonad' Value
type EvalMonad' a = StateT PState (ExceptT RuntimeException IO) a

