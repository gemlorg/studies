module Main
  ( main
  ) where

import           Prelude
import           System.Environment (getArgs)

import           Interpreter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= interpret
    [f] -> readFile f >>= interpret
    _ ->
      putStrLn "Usage: ./interpreter [file] or echo 'progam' | ./interpreter "
