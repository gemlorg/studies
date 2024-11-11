{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module JVM.Compiler where 
import           Grammar.Abs
import           Grammar.Par         (myLexer, pProgram)

import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (hPrint, stderr)
import           Control.Monad.Except
import           Control.Monad.State
import   qualified        Data.Text   as T
import Text.Printf (printf)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map  

import           JVM.Types
import           JVM.Monad
import System.FilePath
import           JVM.Environment

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
          let j = replaceExtension path "j"
          TIO.writeFile j $ T.intercalate (T.pack"\n") $ buildProgram program (filter (not . T.null) res) (takeBaseName path)
          readProcess "java" ["-jar", "lib/jasmin.jar", "-d", takeDirectory j, j] ""
          pure ()

perror :: Show a => a -> IO b
perror s = do
  hPrint stderr s
  exitFailure

compileTree :: Program -> IO (Either CompileException CompileRes)
compileTree program = do
  runExceptT $ evalStateT (compile program) emptyEnv
  

buildProgram :: Program -> [T.Text] -> String -> [T.Text]

buildProgram p res file = prolog file ++ defaultConstructor ++ mainBegin ++ (limits p ) ++ res ++ mainEnd

prolog :: String -> [T.Text]
prolog file = map T.pack [".class public " ++ file,
                          ".super " ++  defaultSuperclass]

limits :: Program -> [T.Text]
limits (Prog _ stmts)  = map T.pack [printf ".limit stack %d" slim]
  where slim = 1 + stackSize stmts

stackSize :: [Stmt] -> Integer
stackSize stmts = foldr max 0 (map stackStmt stmts)

stackStmt :: Stmt -> Integer
stackStmt (SAss _ _ e) = (getExpMap e) Map.! e
stackStmt (SExp  _ e) = (getExpMap e) Map.! e


class Compile a where
  compile :: a -> CompileMonad

instance Compile Program where
  compile :: Program -> CompileMonad
  compile (Prog _ stmts) = do
    res <- mapM compile stmts
    vlim <- gets $ mapSize
    let limit_expr = T.pack $ printf  ".limit locals %d" (1 + vlim)
    pure $ limit_expr: (foldr (++) [] res)

instance Compile Stmt where
  compile (SAss _ var exp) = do
    modify $ updateVar var 
    loc <- gets $ getLoc var
    code <- compile exp
    pure $ code ++ [storeCall loc]
  compile (SExp _ exp) = do 
    res <- compile exp
    pure $ printBegin: res ++ [printEnd]

instance Compile Exp where 
  compile exp = do
    compile (getExpMap exp , exp)

instance Compile (ExpMap, Exp) where
  compile (m ,(ExpVar pos var)) = do 
    isUsed <- gets $ isVarUsed var
    case isUsed of 
      True -> do
        loc <- gets $ getLoc var
        pure [loadCall loc]
      False -> throwError $ Exception (NotInScopeException var) pos
  compile (m, (ExpLit _ n)) = pure [pushCall $ n]
  compile (m, exp) = do 
    let (call, exp1, exp2) = expandExp exp
    code1 <- compile (m, exp1)
    code2 <- compile (m, exp2)
    if (m Map.! exp1) >= (m Map.! exp2) 
      then 
        pure $ code1 ++ code2 ++ [call]
      else if call == T.pack "iadd" || call == T.pack "imul" then
        pure $ code2 ++ code1 ++ [call]
      else pure $ code2 ++ code1 ++ [swapCall, call]

expandExp :: Exp -> (T.Text, Exp, Exp)
expandExp (ExpAdd _ exp1 exp2) = (T.pack "iadd", exp1, exp2)
expandExp (ExpSub _ exp1 exp2) = (T.pack "isub", exp1, exp2)
expandExp (ExpMul _ exp1 exp2) = (T.pack "imul", exp1, exp2)
expandExp (ExpDiv _ exp1 exp2) = (T.pack "idiv", exp1, exp2)
expandExp e = (T.pack "", e,  e)

getExpMap :: Exp -> ExpMap
getExpMap e = case e of
  ExpLit _ _ -> Map.singleton e 1
  ExpVar _ _ -> Map.singleton e 1
  ExpAdd _ e1 e2 -> Map.insert e (computeDepth e1 e2 m) m where
    m = Map.union (getExpMap e1) (getExpMap e2)
  ExpSub _ e1 e2 -> Map.insert e (computeDepth e1 e2 m) m where
    m = Map.union (getExpMap e1) (getExpMap e2)
  ExpMul _ e1 e2 -> Map.insert e (computeDepth e1 e2 m) m where
    m = Map.union (getExpMap e1) (getExpMap e2)
  ExpDiv _ e1 e2 -> Map.insert e (computeDepth e1 e2 m) m where
    m = Map.union (getExpMap e1) (getExpMap e2)



computeDepth :: Exp -> Exp -> ExpMap -> Location
computeDepth e1 e2 m = if a == b then a + 1 else max a b
      where 
        a = (m Map.! e1) 
        b = (m Map.! e2)

storeCall :: Location -> T.Text
storeCall loc = T.pack $ printf "%s%d" istore loc 
  where istore = if loc <= 3 then "istore_" else "istore "

loadCall :: Location -> T.Text
loadCall loc = T.pack $ printf "%s%d" iload loc
  where iload = if loc <= 3 then "iload_" else "iload "

pushCall :: Location -> T.Text
pushCall n = T.pack $ operation n where 
  operation s 
      | s == -1 = "iconst_m1"
      | s >= 0 && s <= 5 = "iconst_" ++ show s
      | s >= -128 && s <= 127 = "bipush " ++ show s
      | s >= -32768 && s <= 32767 = "sipush " ++ show s
      | otherwise = "ldc " ++ show s

swapCall :: T.Text
swapCall = T.pack "swap"

defaultSuperclass =  "java/lang/Object"
mainSignature = T.pack ".method public static main([Ljava/lang/String;)V"
defaultConstructor = map T.pack [".method public <init>()V",
                       "  .limit stack 1",
                       "  .limit locals 1",
                       "  aload_0",
                       "  invokenonvirtual java/lang/Object/<init>()V",
                       "  return",
                       ".end method"]

mainBegin = map T.pack [".method public static main([Ljava/lang/String;)V"]
            
mainEnd = map T.pack ["return",
            ".end method"]

printBegin = T.pack "getstatic java/lang/System/out Ljava/io/PrintStream;"
printEnd = T.pack "invokevirtual java/io/PrintStream/println(I)V"