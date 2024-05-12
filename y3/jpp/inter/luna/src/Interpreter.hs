module Interpreter
    ( interpret
    ) where

import Grammar.Abs 
import Grammar.Par (myLexer, pProgram)
import Grammar.Lex  


import Data.Typeable
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (hPrint, stderr)
import Eval.Eval
import Typecheck.Typecheck ( checkType )


interpret :: String -> IO ()
interpret s = do 
    case parsed of 
        Left err -> perror $ "BNFC Parse error: " ++ err
        Right program -> interpretProgram program
    where parsed = pProgram $ myLexer s

 -- handle errors in typecheck/interpret
interpretProgram :: Program -> IO ()
interpretProgram program = case checkType program of 
    Left e -> do 
        hPrint stderr e
        exitFailure
    Right _ -> do
        res <- evalProgram program
        case res of 
            Left err -> do
                hPrint stderr err
                exitFailure
            Right _ -> exitSuccess


perror :: String -> IO ()
perror s = do  
    hPrint stderr s 
    exitFailure



