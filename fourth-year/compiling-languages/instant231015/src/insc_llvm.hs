import           Prelude
import           System.Environment (getArgs)

import LLVM.Compiler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> compileAndPrint f
    _ ->
      putStrLn "Usage: ./interpreter [file] or echo 'progam' | ./interpreter "
