{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Typecheck.Typecheck
  ( typeCheck
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Grammar.Abs
import           Typecheck.Environment

import           Typecheck.Monad
import           Typecheck.Utils

import           Common.Exception
import           Common.RTypes

typeCheck :: Program -> Either StaticException ()
typeCheck program = runExcept $ evalStateT (checkTypeM Nothing program) emptyEnv

instance Typechecker Program where
  checkTypeM _ (PProgram _ stmts) = do
    mapM_ (checkTypeM Nothing) stmts

instance Typechecker Block where
  checkTypeM expectedReturnType (BBlock _ statements) = do
    mapM_ (checkTypeM expectedReturnType) statements

instance Typechecker DeclKind where
  checkTypeM _ (FDecl position name arguments returnType block) = do
    unless
      (areUniqueArgs arguments)
      (throwError $ Exception (ArgDuplicateException arguments) position)
    env <- get
    put $ updateType env name $ fromFunction arguments returnType
    envWithFunction <- get
    put $ updateTypes envWithFunction $ getArgumentsWithTypes arguments
    checkTypeM (Just (fromType returnType)) block
    blockEnv <- get
    unless (hasReturnStatementOccured blockEnv)
      $ throwError (Exception NoReturnStatementException position)
    put envWithFunction
  checkTypeM _ (VDecl position name exprType item) = do
    env <- get
    case item of
      (Init _ expr) -> do
        case runExpr env expr of
          Left exception -> throwError exception
          Right etype    -> assertTypeC position etype rawExprType
      _ -> pure ()
    put $ updateType env name rawExprType
    where
      rawExprType = fromType exprType

instance Typechecker Stmt where
  checkTypeM _ (Empty _) = pure ()
  checkTypeM expectedReturnType (BStmt _ block) = do
    env <- get
    checkTypeM expectedReturnType block
    put env
  checkTypeM ret (Decl _ declKind) = checkTypeM ret declKind
  checkTypeM _ (Ass position name expr) = do
    env <- get
    case (getType env name, runExpr env expr) of
      (Nothing, _) ->
        throwError $ Exception (UndefinedSymbolException name) position
      (_, Left exception) -> throwError exception
      (Just vtype, Right etype) -> assertTypeC position etype vtype
  checkTypeM _ (Incr position name) = do
    env <- get
    case getType env name of
      Nothing -> throwError $ Exception (UndefinedSymbolException name) position
      Just vtype -> assertTypeC position vtype RTInt
  checkTypeM _ (Decr position name) = do
    env <- get
    case getType env name of
      Nothing -> throwError $ Exception (UndefinedSymbolException name) position
      Just vtype -> assertTypeC position vtype RTInt
  checkTypeM (Just expectedReturnType) (Ret position returnExpr) = do
    env <- get
    case runExpr env returnExpr of
      Right etype    -> assertTypeC position etype expectedReturnType
      Left exception -> throwError exception
    put $ returnStatementOccured env
  checkTypeM Nothing (Ret position _) =
    throwError $ Exception ReturnOutOfScopeException position
  checkTypeM (Just expectedReturnType) (VRet position) = do
    env <- get
    unless (expectedReturnType == RTNil)
      $ throwError
          (Exception (InvalidReturnTypeException expectedReturnType) position)
    put $ returnStatementOccured env
  checkTypeM Nothing (VRet position) =
    throwError $ Exception ReturnOutOfScopeException position
  checkTypeM expectedReturnType (Cond position condition trueBlock) = do
    env <- get
    case runExpr env condition of
      Right etype    -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType trueBlock
  checkTypeM expectedReturnType (CondElse position cond trueBlock falseBlock) = do
    env <- get
    case runExpr env cond of
      Right etype    -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType trueBlock
    checkTypeM expectedReturnType falseBlock
  checkTypeM expectedReturnType (While position cond block) = do
    env <- get
    case runExpr env cond of
      Right etype    -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType block
  checkTypeM _ (SExp position expr) = do
    env <- get
    case runExpr env expr of
      Right etype    -> assertTypeC position etype RTNil
      Left exception -> throwError exception
  checkTypeM expectedReturnType (For position name start finish step block) = do
    env <- get
    case (runExpr env start, runExpr env finish, runExpr env step) of
      (Right startType, Right finishType, Right stepType) -> do
        assertTypeC position startType RTInt
        assertTypeC position finishType RTInt
        assertTypeC position stepType RTInt
        put $ updateType env name RTInt
        checkTypeM expectedReturnType block
      (Left exception, _, _) -> throwError exception
      (_, Left exception, _) -> throwError exception
      (_, _, Left exception) -> throwError exception

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
    case getVarType position name env of
      Right t        -> pure t
      Left exception -> throwError exception
  getTypeM (CApp position expr arguments) = do
    args <- mapM getTypeM arguments
    f <- getTypeM expr
    case f of
      RTFun argumentsTypes returnType -> do
        compareArgs position argumentsTypes args
        pure returnType
      v -> throwError $ Exception (ExpectedFunctionException v) position
    where
      compareArgs ::
           BNFC'Position -> [RawType] -> [RawType] -> EmptyTypegetterMonad
      compareArgs pos expectedArgs actualArgs = do
        if expectedArgs == [RTAny] || expectedArgs == actualArgs
          then pure ()
          else throwError
                 $ Exception
                     (InvalidFunctionArgumentsTypesException
                        expectedArgs
                        actualArgs)
                     pos
  getTypeM (ELambda position arguments returnType block) = do
    unless
      (areUniqueArgs arguments)
      (throwError $ Exception (ArgDuplicateException arguments) position)
    local
      (`updateTypes` argumentsWithTypes)
      (runLocalCheckM arguments returnType block)
    where
      argumentsWithTypes = getArgumentsWithTypes arguments
      runLocalCheckM :: Typechecker a => [Arg] -> Type -> a -> TypegetterMonad
      runLocalCheckM args ret blck = do
        let rawReturnType = fromType ret
        let functionType = fromFunction args ret
        env <- ask
        let checkTypeResult =
              runExcept
                $ evalStateT (checkLambdaBodyM (Just rawReturnType) blck) env
        case checkTypeResult of
          Right _        -> pure ()
          Left exception -> throwError exception
        pure functionType
      checkLambdaBodyM ::
           Typechecker a => Maybe RawType -> a -> TypecheckerMonad
      checkLambdaBodyM expectedType blck = do
        checkTypeM expectedType blck
        blockEnv <- get
        unless
          (hasReturnStatementOccured blockEnv)
          (throwError $ Exception NoReturnStatementException position)
  getTypeM (EApp position name args) = getTypeM (CApp position (EVar position name) args)
