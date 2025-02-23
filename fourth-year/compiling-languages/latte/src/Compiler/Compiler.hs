{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Compiler where 
import           Grammar.Abs

import   qualified        Data.Text   as T

import         Common.Exception

import           Compiler.IR.IR

import Compiler.IR.Pretty (ppllvm) -- Import pretty-printer
import qualified Data.Text.Lazy
import Compiler.Optimizer.Optimizer (optimize)

type CompileRes = T.Text
compileTree :: Program -> IO (Either CompileException CompileRes)
compileTree program = do
    ir  <- getIR program
    case ir of 
        Left e -> pure $ Left e
        Right llvm ->  do 
            llvm' <- optimize llvm 
            pure $ Right $ Data.Text.Lazy.toStrict (ppllvm llvm')

