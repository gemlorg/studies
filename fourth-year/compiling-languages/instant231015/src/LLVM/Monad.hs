module LLVM.Monad where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text            as T
import           Grammar.Abs
import           LLVM.Environment
import           LLVM.Types

type CompileMonad = CompileMonad' CompileRes

type CompileMonad' a = StateT Env (ExceptT CompileException IO) a

getNextId :: CompileMonad' Location
getNextId = do
  currentId <- gets _nextId
  modify (\env -> env {_nextId = currentId + 1})
  return currentId
