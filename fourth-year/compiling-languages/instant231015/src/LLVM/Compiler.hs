{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LLVM.Compiler where 

import           Grammar.Abs
import           Grammar.Par         (myLexer, pProgram)

import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (hPrint, stderr)
import           Control.Monad.Except
import           Control.Monad.State
import   qualified        Data.Text   as T
import Text.Printf (printf)
import qualified Data.Text.IO as TIO

import           Grammar.Abs
import           LLVM.Types
import           LLVM.Monad
import System.FilePath
import           LLVM.Environment
import System.Process



compileAndPrint :: String -> IO ()
compileAndPrint path = do
  s <- readFile path
  let parsed = pProgram $ myLexer s
  case parsed of
    Left err      -> perror $ "BNFC Parse error: " ++ err
    Right program -> do 
      result <- compileTree program
      case result of
        Left e -> perror e
        Right res -> do
          let ll = replaceExtension path "ll"
          -- TIO.putStrLn $ T.pack $ ll
          TIO.writeFile ll $ T.intercalate (T.pack"\n") (prolog ++ main_begin ++ (filter (not . T.null) (snd res)) ++ main_end ++ epilog)
          readProcess "/opt/homebrew/opt/llvm/bin/llvm-as" [ll] ""
          pure ()




perror :: Show a => a -> IO b
perror s = do
  hPrint stderr s
  exitFailure



compileTree :: Program -> IO (Either CompileException CompileRes)
compileTree program = do
  runExceptT $ evalStateT (compile program) emptyEnv

class Compile a where
  compile :: a -> CompileMonad


-- instance Compile Program where 
--   compile :: Program -> CompileMonad 
--   compile _ = pure Dummy

instance Compile Program where
  compile :: Program -> CompileMonad
  compile (Prog _ stmts) = do
    res <- mapM compile stmts
    -- liftIO $ print $ map snd res
    let textParts = map snd res
    pure (dummyLoc, foldr (++) [] textParts)
    -- pure (Ident "", T.pack "")

instance Compile Stmt where
  compile (SAss _ var exp) = do
    isUsed <- gets $ isVarUsed var
    modify $ updateVar var 
    loc <- gets $ getLoc var
    (id, code) <- compile exp
    let allocLine = case isUsed of
          True -> T.pack ""
          False -> allocCall loc 
    let assignLine = assignCall loc id
    pure (dummyLoc, allocLine:code ++ [assignLine])

  compile (SExp _ exp) = do 
    (id, code) <- compile exp
    _ <- getNextId
    pure (dummyLoc, code ++ [printfCall id])

instance Compile Exp where
  compile (ExpVar pos var) = do 
    isUsed <- gets $ isVarUsed var
    case isUsed of 
      True -> do
        loc <- gets $ getLoc var
        nextId <- getNextId
        pure(nextId, [loadCall nextId loc])
      False -> throwError $ Exception (NotInScopeException var) pos
  compile (ExpLit _ int) = do
    nextId <- getNextId
    pure (nextId, [addCall nextId (T.pack $ show 0)(T.pack $ show int)])
  compile exp = do 
    let (call, exp1, exp2) = expandExp exp
    (id1, code1) <- compile exp1
    (id2, code2) <- compile exp2
    nextId <- getNextId
    pure (nextId, code1 ++ code2 ++ [call nextId (varCall id1) (varCall id2)])


expandExp :: Exp -> (Location -> T.Text -> T.Text -> T.Text, Exp, Exp)
expandExp (ExpAdd pos exp1 exp2) = (addCall, exp1, exp2)
expandExp (ExpSub pos exp1 exp2) = (subCall, exp1, exp2)
expandExp (ExpMul pos exp1 exp2) = (mulCall, exp1, exp2)
expandExp (ExpDiv pos exp1 exp2) = (divCall, exp1, exp2)

allocCall :: Location -> T.Text
allocCall loc = T.pack $ printf "%%%d = alloca i32, align 4" loc
assignCall :: Location -> Location -> T.Text
assignCall loc id = T.pack $ printf "store i32 %%%d, i32* %%%d, align 4" id loc
loadCall :: Location -> Location -> T.Text
loadCall loc id = T.pack $ printf "%%%d = load i32, i32* %%%d, align 4" loc id


addCall :: Location -> T.Text -> T.Text -> T.Text
addCall loc a b = T.pack $ printf "%%%d = add nsw i32 %s, %s" loc a b
subCall :: Location -> T.Text -> T.Text -> T.Text
subCall loc a b = T.pack $ printf "%%%d = sub nsw i32 %s, %s" loc a b
mulCall :: Location -> T.Text -> T.Text -> T.Text
mulCall loc a b = T.pack $ printf "%%%d = mul nsw i32 %s, %s" loc a b
divCall :: Location -> T.Text -> T.Text -> T.Text
divCall loc a b = T.pack $ printf "%%%d = sdiv i32 %s, %s" loc a b

varCall :: Location -> T.Text
varCall loc = T.pack $ printf "%%%d" loc

printfCall :: Location -> T.Text
printfCall loc = T.pack $ printf "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %%%d)" loc

prolog :: [T.Text]
prolog = map T.pack ["source_filename = \"test.c\"",
          "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"",
          "target triple = \"x86_64-pc-linux-gnu\"\n",
          "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n"]
main_begin :: [T.Text]         
main_begin = map T.pack  ["define i32 @main() #0 {"]
main_end :: [T.Text]
main_end = map T.pack  ["ret i32 0", "}"]

epilog :: [T.Text]
epilog = map T.pack  ["declare i32 @printf(i8*, ...) #1",
          "attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "attributes #1 = { \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "!llvm.module.flags = !{!0}",
          "!llvm.ident = !{!1}\n",
          "!0 = !{i32 1, !\"wchar_size\", i32 4}",
          "!1 = !{!\"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)\"}"]