module Compiler.Monad where

import           Control.Monad.Except
import           Control.Monad.State
import           Compiler.Types
import           Compiler.Environment


type CompileMonad = CompileMonad' CompileRes
type CompileMonad' a = StateT Env (ExceptT CompileException IO) a

getNextId :: CompileMonad' Location
getNextId = do
  currentId <- gets _nextId
  modify (\env -> env { _nextId = currentId + 1 })
  return currentId