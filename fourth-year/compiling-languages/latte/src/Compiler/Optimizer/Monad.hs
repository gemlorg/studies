module Compiler.Optimizer.Monad where
import Control.Monad.State
import Compiler.Optimizer.Environment
import Control.Monad.Except
import Common.Exception

type OptimizerMonad a = StateT Env IO  a