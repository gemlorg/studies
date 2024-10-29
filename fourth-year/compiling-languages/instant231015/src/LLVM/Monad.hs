module LLVM.Monad where

import           Control.Monad.Except
import           Control.Monad.State
import           LLVM.Types
import           LLVM.Environment
import           Grammar.Abs
import qualified Data.Text as T

type CompileMonad = CompileMonad' CompileRes
type CompileMonad' a = StateT Env (ExceptT CompileException IO) a

-- getNextId :: CompileMonad Int
-- getNextId = do
--   currentId <- gets _nextId
--   modify (\env -> env { _nextId = currentId + 1 })
--   return currentId
getNextId :: CompileMonad' Location
getNextId = do
  currentId <- gets _nextId
  modify (\env -> env { _nextId = currentId + 1 })
  return currentId