{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Typecheck.Typecheck where

import Grammar.Abs


import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State


import          Typecheck.Environment
import          Typecheck.Exception
import          Typecheck.Monad
import         Typecheck.Utils
import           Control.Monad.Reader


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
    unless (areUniqueArgs arguments) (throwError $ NamesDuplicationException position)

    env <- get
    let functionType = fromFunction arguments returnType
    let rawReturnType = fromType returnType
    put $ updateType env name functionType

    envWithFunction <- get
    let argumentsWithTypes = getArgumentsWithTypes arguments
    put $ updateTypes envWithFunction argumentsWithTypes
    checkTypeM (Just rawReturnType) block
    blockEnv <- get
    unless (hasReturnStatementOccured blockEnv) $ throwError (NoReturnStatementException position)

    put envWithFunction


  checkTypeM _ (VDecl position name exprType item) = do
    env <- get
    let rawExprType = fromType exprType
    case item of
        (Init _ expr) -> do
            let res = runExpr env expr
            case res of
                Left exception -> throwError exception
                Right etype -> assertTypeC position etype rawExprType
        _ -> pure ()
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
    let v = getType env name
    let e = runExpr env expr
    case (v, e) of
      (Nothing, _) -> throwError $ UndefinedSymbolException position name
      (_, Left exception) -> throwError exception
      (Just vtype, Right etype) -> assertTypeC position etype vtype


  checkTypeM _ (Incr position name) = do
    env <- get
    let v = getType env name
    case v of
      Nothing -> throwError $ UndefinedSymbolException position name
      Just vtype -> assertTypeC position vtype RTInt

  checkTypeM _ (Decr position name) = do
    env <- get
    let v = getType env name
    case v of
      Nothing -> throwError $ UndefinedSymbolException position name
      Just vtype -> assertTypeC position vtype RTInt

  checkTypeM (Just expectedReturnType) (Ret position returnExpr) = do
    env <- get

    let actual = runExpr env returnExpr
    case actual of
      Right etype -> assertTypeC position etype expectedReturnType
      Left exception -> throwError exception
    -- let typecheckResult = TCU.expectTypeM position expectedReturnType returnExpr env
    -- TCU.parseReturnTypecheckResultM position expectedReturnType typecheckResult
    put $ returnStatementOccured env


  checkTypeM Nothing (Ret position _) =
    throwError $ ReturnOutOfScopeException position

  checkTypeM (Just expectedReturnType) (VRet position) = do
    env <- get
    unless (expectedReturnType == RTNil) $ throwError (InvalidReturnTypeException position expectedReturnType)
    put $ returnStatementOccured env

  checkTypeM Nothing (VRet position) =
    throwError $ ReturnOutOfScopeException position

  checkTypeM expectedReturnType (Cond position cond trueBlock) = do
    env <- get
    let ex = runExpr env cond
    case ex of
      Right etype -> assertTypeC position etype RTBool
      Left exception -> throwError exception

    checkTypeM expectedReturnType trueBlock

  checkTypeM expectedReturnType (CondElse position cond trueBlock falseBlock) = do
    env <- get
    let ex = runExpr env cond
    case ex of
      Right etype -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType trueBlock
    checkTypeM expectedReturnType falseBlock

  checkTypeM expectedReturnType (While position cond block) = do
    env <- get
    let ex = runExpr env cond
    case ex of
      Right etype -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType block

  checkTypeM _ (SExp position expr) = do
    env <- get
    let ex = runExpr env expr
    case ex of
      Right etype -> assertTypeC position etype RTNil
      Left exception -> throwError exception

  checkTypeM _ _ = error "Not implemented"


instance Typegetter Expr where

  getTypeM (ELitInt _ _) = pure RTInt

  getTypeM (ELitTrue _) = pure RTBool

  getTypeM (ELitFalse _) = pure RTBool

  getTypeM (EString _ _) = pure RTString

  getTypeM (Neg position expr) = do
    t <- getTypeM expr
    assertTypeG position t RTInt
    pure RTInt

  getTypeM (Not position expr) = do
    t <- getTypeM expr
    assertTypeG position t RTBool
    pure RTBool

  getTypeM (EMul position expr1 _ expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertTypeG position t1 RTInt
    assertTypeG position t2 RTInt
    pure RTInt

  getTypeM (EAdd position expr1 _ expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertTypeG position t1 RTInt
    assertTypeG position t2 RTInt
    pure RTInt

  getTypeM (ERel position expr1 _ expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertTypeG position t1 RTInt
    assertTypeG position t2 RTInt
    pure RTBool

  getTypeM (EAnd position expr1 expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertTypeG position t1 RTBool
    assertTypeG position t2 RTBool
    pure RTBool

  getTypeM (EOr position expr1 expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertTypeG position t1 RTBool
    assertTypeG position t2 RTBool
    pure RTBool

  getTypeM (EVar position name) = do
    env <- ask
    let v = getVarType position name env
    case v of
      Right t -> pure t
      Left exception -> throwError exception

  getTypeM (EApp position name arguments) = do
    env <- ask
    let fun = getVarType position name env
    args <- mapM getTypeM arguments

    case fun of
      Right (RTFun argumentsTypes returnType) -> do
        compareArgs position argumentsTypes args
        pure returnType
      Right f-> throwError $ ExpectedFunctionException position f
      Left exception -> throwError exception
    where
      compareArgs :: BNFC'Position -> [RawType] -> [RawType] -> EmptyTypegetterM
      compareArgs pos expectedArgs actualArgs = do
        if expectedArgs == [RTAny] || expectedArgs == actualArgs
          then pure ()
          else throwError $ InvalidFunctionArgumentsTypesException pos expectedArgs actualArgs

  -- getTypeM (EApp position name arguments) = do
  --   TGU.getDefinedSymbolOrThrowM position name (TGU.getFunctionTypeOrThrowM position arguments)



  getTypeM (ELambda position arguments returnType block) = do

      unless (areUniqueArgs arguments) (throwError $ NamesDuplicationException position)
      let argumentsWithTypes = getArgumentsWithTypes arguments

      -- env <- ask
      local (`updateTypes` argumentsWithTypes) (runLocalCheckM arguments returnType block)

    where
      runLocalCheckM :: Typechecker a => [Arg] -> Type -> a -> TypegetterM
      runLocalCheckM args ret blck = do
        let rawReturnType = fromType ret
        let functionType = fromFunction args ret

        env <- ask
        let checkTypeResult = runExcept $ evalStateT (checkLambdaBodyM (Just rawReturnType) blck) env
        case checkTypeResult of
          Right _ -> pure ()
          Left exception -> throwError exception

        pure functionType

      checkLambdaBodyM :: Typechecker a => Maybe RawType -> a -> TypecheckerM
      checkLambdaBodyM expectedType blck = do
        checkTypeM expectedType blck
        blockEnv <- get
        unless (hasReturnStatementOccured blockEnv) (throwError $ NoReturnStatementException position)
  getTypeM _ = pure RTNil



runExpr ::  Env -> Expr -> Either TypecheckingException RawType
runExpr env expr = runExcept $ runReaderT (getTypeM expr) env
