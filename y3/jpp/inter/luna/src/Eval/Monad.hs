
module Eval.Monad where

import           Eval.Environment
import           Eval.Exception

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State


type EvalT = EvalT' Value
type EvalT' a = StateT PState (ExceptT RuntimeException IO) a

