{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

module Eval.Eval where 

import           Grammar.Abs
import Common.Types

import           Common.Exception
import           Common.Types
import           Eval.Environment
import           Eval.Utils
import           Eval.Exception
import           Eval.Monad

import           Control.Lens                 hiding (Empty)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad



evalProgram :: Program -> IO (Either RuntimeException Value)
evalProgram program = do 
    res <- runExceptT $ execStateT (eval program) emptyPState 
    print $ "res: "

    case res of 
        Right pers -> print $ show pers
        Left err -> error "error"

    return $ Right Dummy

    


class Eval a where
  eval :: a -> EvalT
  
instance Eval Program where
    eval :: Program -> EvalT
    eval (PProgram position stmts) = do
        mapM_ eval stmts
        pure Dummy

instance Eval DeclKind where 
    eval (VDecl  _ ident typeOpt item) = do
        case item of 
            (NoInit _) -> do 
                modify $ putValue ident VNil
                pure Dummy
            (Init _ expr) -> do 
                value <- eval expr
                modify $ putValue ident value
                pure Dummy
    eval (FDecl _ ident args returnTypeOpt block) = do
        pers <- get
        let f = VFun args block (_env pers)
        modify $ putValue ident f
        pure Dummy

instance Eval Block where 
    eval (BBlock _ stmts) = do
        mapM_ eval stmts
        pure Dummy
    
instance Eval Expr where
    eval (ELitInt _ int) = pure $ VInt int
    eval (ELitTrue _) = pure $ VBool True
    eval (ELitFalse _) = pure $ VBool False
    eval (EString _ str) = pure $ VStr str
    eval (ENil _) = pure VNil
    eval (EVar _ name) = gets $ getValue name
    eval (EAny _) = pure VNil

    eval (Neg _ expr) = do
        value <- eval expr
        pure $ applyToInt ((-1)*) value
    eval (Not _ expr) = do 
        value <- eval expr
        pure $ applyToBool not value
    eval (EMul _ epr1 op expr2) = do 
        value1 <- eval epr1
        value2 <- eval expr2
        case op of 
            Times _ -> pure $ intOp (*) value1 value2
            Div _ -> if value2 == VInt 0 then throwError $ DivideByZeroException NoPos else pure $ intOp div value1 value2
            Mod _ -> if value2 == VInt 0 then throwError $ DivideByZeroException NoPos else pure $ intOp mod value1 value2
    eval (EAdd _ expr1 op expr2) = do
        value1 <- eval expr1
        value2 <- eval expr2
        case op of 
            Plus _ -> pure $ intOp (+) value1 value2
            Minus _ -> pure $ intOp (-) value1 value2

    eval (ERel _ expr1 op expr2) = do
        value1 <- eval expr1
        value2 <- eval expr2
        case op of 
            LTH _ -> pure $ relOp (<) value1 value2
            LE _ -> pure $ relOp (<=) value1 value2
            GTH _ -> pure $ relOp (>) value1 value2
            GE _ -> pure $ relOp (>=) value1 value2
            EQU _ -> pure $ relOp (==) value1 value2
            NE _ -> pure $ relOp (/=) value1 value2
    eval (EAnd _ expr1 expr2) = do
        value1 <- eval expr1
        value2 <- eval expr2
        pure $ boolOp (&&) value1 value2
    eval (EOr _ expr1 expr2) = do
        value1 <- eval expr1
        value2 <- eval expr2
        pure $ boolOp (||) value1 value2

    eval (ELambda _ arguments _ block) = gets (VFun arguments block . getEnv)
    

    eval (EApp _ ident args) = do 
        pers <- get 

        argVals <- mapM eval args
        -- if function name is print then print and return, otherwise do the rest 
        if ident == printLabel then do
            printFun argVals
        else do
            argLocs <- mapM getExprLoc args

            let savedEnv =  getEnv pers
            let fval = getValue ident pers
            let (argNames, block, fenv) = toFun  fval
            
            modify $ putEnv fenv
            modify $ putValue ident fval
            modify $ putValue returnLabel VNil
            mapM_ insertArgs (zip3 argNames argVals argLocs)

            eval block 
            pers <- get
            let returnValue = getValue returnLabel pers

            modify $ putEnv savedEnv
            pure returnValue

        where getExprLoc :: Expr -> EvalT' Location
              getExprLoc (EVar _ name) =gets $ getSafeLocation name
              getExprLoc _ = pure (-1)
            
              toFun (VFun argNames block fenv) = (argNames, block, fenv)
              toFun _ = error "not a function, cannot eval"
        


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
        let newvalue = applyToInt (1+) value
        modify $ updateValue ident newvalue 
        pure Dummy

    eval (Decr _ ident) = do
        pers <- get
        let value = getValue ident pers
        let newvalue = applyToInt (1-) value
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
            (VBool True) -> eval stmt1
            (VBool False) -> eval stmt2
            _ -> pure Dummy

    eval (While _ expr stmt) = do 
        exprValue <- eval expr
        case exprValue of
            (VBool True) -> do
                _ <- eval stmt
                eval (While NoPos expr stmt)
            _ -> pure Dummy
    
    eval _ = error "not implemented"

