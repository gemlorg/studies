{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval.Eval where

import           Common.GrammarUtils
import           Eval.Values
import           Grammar.Abs

import           Common.Exception
import           Control.Monad.Except
import           Control.Monad.State
import           Eval.Environment
import           Eval.Monad
import           Eval.Utils           (checkReturn, insertArgs, printFun)
import           Control.Lens   hiding (Empty, op, index)

evalProgram :: Program -> IO (Either RuntimeException Value)
evalProgram program = do
  runExceptT $ evalStateT (eval program) emptyPState

class Eval a where
  eval :: a -> EvalMonad

instance Eval Program where
  eval :: Program -> EvalMonad
  eval (PProgram _ stmts) = do
    mapM_ eval stmts
    pure Dummy

instance Eval DeclKind where
  eval :: DeclKind -> EvalMonad
  eval (VDecl _ ident _ item) = do
    case item of
      (NoInit _) -> do
        modify $ putValue ident VNil
        pure Dummy
      (Init _ expr) -> do
        value <- eval expr
        modify $ putValue ident value
        pure Dummy
  eval (FDecl _ ident args _ block) = do
    pers <- get
    let f = VFun ident args block (_env pers)
    modify $ putValue ident f
    pure Dummy

instance Eval Block where
  eval (BBlock _ stmts) = do
    mapM_ (checkReturn . eval) stmts
    pure Dummy

instance Eval Expr where
  eval (ELitInt _ int) = pure $ VInt int
  eval (ELitTrue _) = pure $ VBool True
  eval (ELitFalse _) = pure $ VBool False
  eval (EString _ str) = pure $ VStr str
  eval (EVar _ name) = gets $ getValue name
  eval (Neg _ expr) = do
    value <- eval expr
    pure $ applyToInt ((-1) *) value
  eval (Not _ expr) = do
    value <- eval expr
    pure $ applyToBool not value
  eval (EMul pos epr1 op expr2) = do
    value1 <- eval epr1
    value2 <- eval expr2
    case op of
      Times _ -> pure $ intOp (*) value1 value2
      Div _ ->
        if value2 == VInt 0
          then throwError (Exception DivideByZeroException pos)
          else pure $ intOp div value1 value2
      Mod _ ->
        if value2 == VInt 0
          then throwError (Exception DivideByZeroException pos)
          else pure $ intOp mod value1 value2
  eval (EAdd _ expr1 op expr2) = do
    value1 <- eval expr1
    value2 <- eval expr2
    case op of
      Plus _  -> pure $ intOp (+) value1 value2
      Minus _ -> pure $ intOp (-) value1 value2
  eval (ERel _ expr1 op expr2) = do
    value1 <- eval expr1
    value2 <- eval expr2
    case op of
      LTH _ -> pure $ relOp (<) value1 value2
      LE _  -> pure $ relOp (<=) value1 value2
      GTH _ -> pure $ relOp (>) value1 value2
      GE _  -> pure $ relOp (>=) value1 value2
      EQU _ -> pure $ relOp (==) value1 value2
      NE _  -> pure $ relOp (/=) value1 value2
  eval (EAnd _ expr1 expr2) = do
    value1 <- eval expr1
    value2 <- eval expr2
    pure $ boolOp (&&) value1 value2
  eval (EOr _ expr1 expr2) = do
    value1 <- eval expr1
    value2 <- eval expr2
    pure $ boolOp (||) value1 value2
  eval (ELambda _ arguments _ block) =
    gets (VFun lambdaLabel arguments block . view env)
  eval (CApp _ expr args) = do
    
    pers <- get
    argVals <- mapM eval args
    fval <- eval expr
    if fval == Print
      then do
        printFun argVals
      else do
        
        argLocs <- mapM getExprLoc args
        let savedEnv = view env pers
        let (ident, argNames, block, fenv) = toFun fval
        modify $ set env fenv
        modify $ putValue ident fval
        modify $ putValue returnLabel Dummy
        mapM_ insertArgs (zip3 argNames argVals argLocs)
        _ <- eval block
        pstate <- get
        let returnValue = getValue returnLabel pstate
        modify $ set env  savedEnv
        pure returnValue
    where
      getExprLoc :: Expr -> EvalMonad' Location
      getExprLoc (EVar _ name) = gets $ getSafeLocation name
      getExprLoc _             = pure (-1)
      toFun (VFun name argNames block fenv) = (name, argNames, block, fenv)
      toFun _ = error "not a function, cannot eval"

  eval (EApp _ ident args) = eval (CApp NoPos (EVar NoPos ident) args)
  
instance Eval Stmt where
  eval (Empty _) = pure Dummy
  eval (BStmt _ block) = eval block
  eval (Decl _ declKind) = eval declKind
  eval (SExp _ expr) = eval expr
  eval (Ass _ ident expr) = do
    newvalue <- eval expr
    modify $ updateValue ident newvalue
    pure Dummy
  eval (Incr _ ident) = do
    pers <- get
    let value = getValue ident pers
    let newvalue = applyToInt (1 +) value
    modify $ updateValue ident newvalue
    pure Dummy
  eval (Decr _ ident) = do
    pers <- get
    let value = getValue ident pers
    let newvalue = applyToInt (1 -) value
    modify $ updateValue ident newvalue
    pure Dummy
  eval (Ret _ expr) = do
    value <- eval expr
    modify $ updateValue returnLabel value
    pure Dummy
  eval (VRet _) = do
    modify $ updateValue returnLabel VNil
    pure Dummy
  eval (Cond pos expr stmt) = do
    eval (CondElse pos expr stmt (Empty NoPos))
  eval (CondElse _ expr stmt1 stmt2) = do
    value <- eval expr
    case value of
      (VBool True)  -> eval stmt1
      (VBool False) -> eval stmt2
      _             -> pure Dummy
  eval (While _ expr stmt) = do
    exprValue <- eval expr
    case exprValue of
      (VBool True) -> do
        _ <- eval stmt
        eval (While NoPos expr stmt)
      _ -> pure Dummy
  eval (For position ident start finish step block) = do
    startValue <- eval start
    finishValue <- eval finish
    stepValue <- eval step
    let index = valToInt stepValue
    _ <-
      case index of
        0 -> throwError (Exception InvalidStepException position)
        _ -> pure Dummy
        -- let iLoc = getSafeLocation ident
    modify $ putValue ident startValue
    _ <- evalFor startValue finishValue stepValue
        --put original value back
    pure Dummy
    where
      evalFor i e s = do
        if checkCond (valToInt i) (valToInt e) (valToInt s)
          then do
            _ <- eval block
            let i' = applyToInt (+ valToInt s) i
            modify $ updateValue ident i'
            evalFor i' e s
          else pure Dummy

checkCond :: Integer -> Integer -> Integer -> Bool
checkCond start finish step =
  if step > 0
    then start <= finish
    else start >= finish
