{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Typecheck.Typecheck where 

import Grammar.Abs

import           Control.Lens hiding (Empty)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe                   (isNothing)


import          Typecheck.Environment
import          Typecheck.Exception
import          Typecheck.Monad
import         Typecheck.Utils
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State


import Common.Exception 
import Common.Types
import qualified Typecheck.TC as TCU
import qualified Typecheck.TG  as TGU


checkType :: Program -> Either TypecheckingException ()
checkType program =
  runExcept $ evalStateT (checkTypeM Nothing program) emptyEnv


instance Typechecker Program where

  checkTypeM _ (PProgram _ stmts) = do
      mapM_ (checkTypeM Nothing) stmts

instance Typechecker Block where

  checkTypeM expectedReturnType (BBlock _ statements) = do
    mapM_ (checkTypeM expectedReturnType) statements

instance Typechecker DeclKind where
  checkTypeM _ (FDecl position name arguments returnType block) = do
    TCU.expectValidFunctionArgumentsOrThrowM position arguments

    env <- get
    let functionType = fromFunction arguments returnType
    let rawReturnType = fromType returnType
    put $ updateType env name functionType

    envWithFunction <- get
    let argumentsWithTypes = getArgumentsWithTypes arguments
    put $ updateTypes envWithFunction argumentsWithTypes
    checkTypeM (Just rawReturnType) block
    blockEnv <- get
    TCU.assertOrThrowM (hasReturnStatementOccured blockEnv) (NoReturnStatementException position)

    put envWithFunction

-- ident typeOpt item
  checkTypeM _ (VDecl position name exprType item) = do
    case item of 
        (NoInit _) -> do 
            env <- get
            put $ updateType env name (fromType exprType)
        (Init _ expr) -> do 
            -- value <- eval expr
            -- modify $ putValue name value
            -- pure Dummy
            let rawExprType = fromType exprType
            env <- get
            TCU.expectTypeOrThrowM position env rawExprType expr
            put $ updateType env name rawExprType

instance Typechecker Stmt where

  checkTypeM _ (Empty _) = pure ()

  checkTypeM expectedReturnType (BStmt _ block) = do
    env <- get
    checkTypeM expectedReturnType block
    put env

  checkTypeM ret (Decl _ declKind) =
    checkTypeM ret declKind

  checkTypeM _ (Ass position name expr) = do
    
    env <- get
    case getType env name of
      Just rawType -> TCU.expectTypeOrThrowM position env rawType expr
      Nothing      -> throwError $ UndefinedSymbolException position name

  checkTypeM _ (Incr position name) =
    TCU.expectSymbolTypeOrThrowM position RTInt name

  checkTypeM _ (Decr position name) =
    TCU.expectSymbolTypeOrThrowM position RTInt name

  checkTypeM (Just expectedReturnType) (Ret position returnExpr) = do
    env <- get
    let typecheckResult = TCU.expectTypeM position expectedReturnType returnExpr env
    TCU.parseReturnTypecheckResultM position expectedReturnType typecheckResult
    put $ returnStatementOccured env

  checkTypeM Nothing (Ret position _) =
    throwError $ ReturnOutOfScopeException position

  checkTypeM (Just expectedReturnType) (VRet position) = do
    env <- get
    TCU.assertOrThrowM (expectedReturnType == RTNil) (InvalidReturnTypeException position expectedReturnType)
    put $ returnStatementOccured env

  checkTypeM Nothing (VRet position) =
    throwError $ ReturnOutOfScopeException position

  checkTypeM expectedReturnType (Cond position cond trueBlock) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTBool cond
    checkTypeM expectedReturnType trueBlock

  checkTypeM expectedReturnType (CondElse position cond trueBlock falseBlock) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTBool cond
    checkTypeM expectedReturnType trueBlock
    checkTypeM expectedReturnType falseBlock

  checkTypeM expectedReturnType (While position cond block) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTBool cond
    checkTypeM expectedReturnType block

  checkTypeM _ (SExp position expr) = do
    env <- get
    TCU.expectTypeOrThrowM position env RTNil expr

  checkTypeM _ _ = error "Not implemented"


instance Typegetter Expr where

  getTypeM (ELitInt _ _) = pure RTInt

  getTypeM (ELitTrue _) = pure RTBool

  getTypeM (ELitFalse _) = pure RTBool

  getTypeM (EString _ _) = pure RTString

  getTypeM (Neg position expr) = do
    t <- getTypeM expr 
    assertType position t RTInt
    pure RTInt

  getTypeM (Not position expr) = do
    t <- getTypeM expr 
    assertType position t RTBool
    pure RTBool

  getTypeM (EMul position expr1 _ expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertType position t1 RTInt
    assertType position t2 RTInt
    pure RTInt

  getTypeM (EAdd position expr1 _ expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertType position t1 RTInt
    assertType position t2 RTInt
    pure RTInt

  getTypeM (ERel position expr1 _ expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertType position t1 RTInt
    assertType position t2 RTInt
    pure RTBool

  getTypeM (EAnd position expr1 expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertType position t1 RTBool
    assertType position t2 RTBool
    pure RTBool

  getTypeM (EOr position expr1 expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertType position t1 RTBool
    assertType position t2 RTBool
    pure RTBool

  getTypeM (EVar position name) =
    getVarType position name

  getTypeM (EApp position name arguments) = do
    fun <- getVarType position name
    args <- mapM getTypeM arguments
    
    case fun of
      RTFun argumentsTypes returnType -> do
        compareArgs position argumentsTypes args
        pure returnType
      _ -> throwError $ ExpectedFunctionException position fun
    where 
      compareArgs :: BNFC'Position -> [RawType] -> [RawType] -> EmptyTypegetterM
      compareArgs pos expectedArgs actualArgs = do
        if expectedArgs == [RTAny] || expectedArgs == actualArgs
          then pure ()
          else throwError $ InvalidFunctionArgumentsTypesException pos expectedArgs actualArgs

    
  

  getTypeM (ELambda position arguments returnType block) = do

      TGU.expectValidFunctionArgumentsOrThrowM position arguments
      let argumentsWithTypes = getArgumentsWithTypes arguments

      env <- ask
      local (`updateTypes` argumentsWithTypes) (runLocalCheckM arguments returnType block)

    where
      runLocalCheckM :: Typechecker a => [Arg] -> Type -> a -> TypegetterM
      runLocalCheckM arguments returnType block = do
        let rawReturnType = fromType returnType
        let functionType = fromFunction arguments returnType

        env <- ask
        TGU.checkTypeOrThrowM env (Just rawReturnType) block checkLambdaBodyM
        pure functionType

      checkLambdaBodyM :: Typechecker a => Maybe RawType -> a -> TypecheckerM
      checkLambdaBodyM expectedType block = do
        checkTypeM expectedType block
        blockEnv <- get
        TCU.assertOrThrowM (hasReturnStatementOccured blockEnv) (NoReturnStatementException position)
  getTypeM _ = pure RTNil
   


