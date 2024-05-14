module Eval.Utils where

import      Grammar.Abs
import       Eval.Monad
import       Eval.Environment
import Eval.Values
import  Control.Monad.State


insertArgs :: (Arg, Value, Location) -> EvalMonad
insertArgs (AArg _ name _, value, _) = do
  modify $ putValue name value
  pure Dummy

insertArgs (AArgVar _ name _, value, -1) = do
   modify $ putValue name value
   pure Dummy

insertArgs (AArgVar _ name _, _, location) = do
  modify $ putLocation name location
  pure Dummy

printFun ::  [Value] -> EvalMonad
printFun vals = do
    liftIO $ putStrLn $ unwords (map showValue vals)
    pure Dummy

    
checkReturn :: EvalMonad -> EvalMonad
checkReturn exec = do 
    pers <- get
    if hasReturned pers then pure Dummy else exec