module Eval.Utils where

import       Eval.Monad 
import       Eval.Environment
import      Grammar.Abs
import Eval.Exception
import           Control.Monad.Except
import  Control.Monad.State
import Data.List (intercalate)

insertArgs :: (Arg, Value, Location) -> EvalT
insertArgs (AArg _ name _, value, _) = do
  modify $ putValue name value
  pure Dummy

insertArgs (AArgVar position name _, value, -1) = do
   modify $ putValue name value
   pure Dummy
--   throwError $ InvalidReferenceFunctionArgumentAplicationException position

insertArgs (AArgVar _ name _, _, location) = do
  modify $ putLocation name location
  pure Dummy

printFun ::  [Value] -> EvalT
printFun vals = do 
    liftIO $ putStrLn $ intercalate " " (map showValue vals)
    pure Dummy
