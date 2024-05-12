module Typecheck.TG
  ( expectTypesOrThrowM
  , expectTypeOrThrowM
  , checkTypeOrThrowM
  , getDefinedSymbolOrThrowM
  , getFunctionTypeOrThrowM
  , expectValidFunctionArgumentsOrThrowM
  , assertTypeOrThrowM
  , assertOrThrowM
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import          Control.Monad.IO.Class
import           Data.List
import           Prelude
import           Grammar.Abs
import           Typecheck.Environment
import           Typecheck.Exception
import           Typecheck.Monad
import          Typecheck.Utils


expectTypesOrThrowM :: Typegetter a => BNFC'Position -> RawType -> a -> a -> EmptyTypegetterM
expectTypesOrThrowM position expectedType expr1 expr2 = do
  expectTypeOrThrowM position expectedType expr1
  expectTypeOrThrowM position expectedType expr2

expectTypeOrThrowM :: Typegetter a => BNFC'Position -> RawType -> a -> EmptyTypegetterM
expectTypeOrThrowM position expectedType expr = do
  exprType <- getTypeM expr
  assertTypeOrThrowM expectedType exprType (InvalidTypeException position expectedType exprType)


checkTypeOrThrowM :: Typechecker a => Env -> Maybe RawType -> a -> (Maybe RawType -> a -> TypecheckerM) -> EmptyTypegetterM
checkTypeOrThrowM env expectedType block executor = do
  let checkTypeResult = runExcept $ evalStateT (executor expectedType block) env
  parseCheckTypeResultM checkTypeResult

parseCheckTypeResultM :: Either TypecheckingException () -> EmptyTypegetterM
parseCheckTypeResultM (Right _)        = pure ()
parseCheckTypeResultM (Left exception) = throwError exception


getDefinedSymbolOrThrowM :: BNFC'Position -> Ident -> (RawType -> TypegetterM) -> TypegetterM
getDefinedSymbolOrThrowM  position name callback = do
  env <- ask
  case getType env name of
    Just symbolType -> callback symbolType
    Nothing         -> throwError $ UndefinedSymbolException position name


getFunctionTypeOrThrowM :: Typegetter a => BNFC'Position -> [a] -> RawType -> TypegetterM
getFunctionTypeOrThrowM position actualArgs (RTFun expectedArgTypes returnType) = do
  actualArgsTypes <- mapM getTypeM actualArgs
  expectFunctionArgumentsOrThrowM position expectedArgTypes actualArgsTypes
  pure returnType
getFunctionTypeOrThrowM position _ actualType =
  throwError $ ExpectedFunctionException position actualType

expectFunctionArgumentsOrThrowM :: BNFC'Position -> [RawType] -> [RawType] -> EmptyTypegetterM
expectFunctionArgumentsOrThrowM position expectedArguments actualArguments = assertOrThrowM isValidType exception
  where

    isValidType = (expectedArguments == [RTAny]) || (expectedArguments == actualArguments)

    exception = InvalidFunctionArgumentsTypesException position expectedArguments actualArguments


expectValidFunctionArgumentsOrThrowM :: BNFC'Position -> [Arg] -> EmptyTypegetterM
expectValidFunctionArgumentsOrThrowM position arguments = do
  let areArgumentsValid = validateFunctionArguments arguments
  assertOrThrowM areArgumentsValid (FunctionArgumentsNameDuplicationException position arguments)


assertTypeOrThrowM :: Eq a => a -> a -> TypecheckingException -> EmptyTypegetterM
assertTypeOrThrowM expectedType actualType = assertOrThrowM isValidType
  where
    isValidType = expectedType == actualType

assertOrThrowM :: Bool -> TypecheckingException -> EmptyTypegetterM
assertOrThrowM True _          = pure ()
assertOrThrowM False exception = throwError exception

