module Interpreter
  ( interpret
  ) where

import           Grammar.Abs
import           Grammar.Par         (myLexer, pProgram)

import           Eval.Eval
import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (hPrint, stderr)
import           Typecheck.Typecheck (typeCheck)

interpret :: String -> IO ()
interpret s = do
  case parsed of
    Left err      -> perror $ "BNFC Parse error: " ++ err
    Right program -> interpretProgram program
  where
    parsed = pProgram $ myLexer s

interpretProgram :: Program -> IO ()
interpretProgram program =
  case typeCheck program of
    Left e -> perror e
    Right _ -> do
      res <- evalProgram program
      case res of
        Left err -> perror err
        Right _  -> exitSuccess

perror :: Show a => a -> IO b
perror s = do
  hPrint stderr s
  exitFailure
