{-# LANGUAGE FlexibleInstances #-}
module Eval.Exception where

import           Grammar.Abs

import           Common.Exception
import           Eval.Environment

type RuntimeException = RuntimeException' BNFC'Position

data RuntimeException' a
  = UnknownRuntimeException a
  | InvalidReferenceFunctionArgumentAplicationException a
  | DivideByZeroException a



instance Show RuntimeException where

  show (UnknownRuntimeException position) =
    "RUNTIME EXCEPTION: Unknown exception! At " ++ showPos position

  show (InvalidReferenceFunctionArgumentAplicationException position) =
    "RUNTIME EXCEPTION: Invalid reference function argument! Argument has to be variable! At " ++ showPos position

  show (DivideByZeroException position) =
    "RUNTIME EXCEPTION: Divide by zero! At " ++ showPos position


showPos :: BNFC'Position -> String
showPos (Just (line, column)) = concat ["line ", show line, ", column ", show column]
showPos _ = "unknow"

