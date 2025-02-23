import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Exit          (exitFailure, exitSuccess)
import           System.FilePath
import           System.IO            (hPrint, stderr)
import           System.Environment (getArgs)
import           System.Process
import           Text.Printf          (printf)

import           Grammar.Abs
import           Grammar.Par         (myLexer, pProgram)
import Control.Exception (try, catch)
import          Typechecker.Typechecker (typeCheck)
import          Compiler.Compiler    (compileTree)


perror :: Show a => a -> IO b
perror s = do
  hPrint stderr "ERROR"
  hPrint stderr s
  exitFailure

noerror :: IO b 
noerror = do 
  hPrint stderr "OK"
  exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> compileAndPrint f
    _ ->
      perror $  "Usage: ./latc_x86_64 [file].lat"

compileAndPrint :: String -> IO ()
compileAndPrint path = do
  case takeExtension path of
    ".lat" -> pure ()
    _ -> perror $ "File must have .lat extension"
  s <- try (readFile path) :: IO (Either IOError String)
  case s of 
    Left e -> perror e
    Right s -> do
      let parsed = pProgram $ myLexer s
      case parsed of
        Left err -> perror $ "BNFC Parse error: " ++ err
        Right program -> do 
          case typeCheck program of 
            Left err -> perror err
            Right _ -> do
              res <- compileTree program
              case res of 
                Left err -> perror err
                Right res -> do 
                  let llFile = replaceExtension path "ll"
                  let bcFile = replaceExtension path "bc"
                  let execFile = replaceExtension path ""
                  -- flush results 
                  TIO.writeFile llFile  res
                  readProcess "llvm-link" ["-o", bcFile, llFile, "./lib/runtime.ll"] ""
                  readProcess "clang" ["-o", execFile, bcFile] ""
                  noerror
