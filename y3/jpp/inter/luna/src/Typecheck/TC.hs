module Typecheck.TC
  ( expectTypeOrThrowM
  , expectTypeM
  , expectSymbolTypeOrThrowM
  , parseReturnTypecheckResultM
  , parseTypecheckResultM
  , expectValidFunctionArgumentsOrThrowM
  , assertOrThrowM
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Prelude
import           Grammar.Abs
import           Typecheck.Environment
import           Typecheck.Utils
import Typecheck.Exception
    ( TypecheckingException,
      TypecheckingException'(FunctionArgumentsNameDuplicationException,
                             InvalidTypeException, UndefinedSymbolException,
                             NamesDuplicationException) )
import           Typecheck.Monad
import  qualified  Typecheck.TG as TGU



expectTypeOrThrowM :: Typegetter a => BNFC'Position -> Env -> RawType -> a -> TypecheckerM
expectTypeOrThrowM position env expectedType expr = do
  let typecheckResult = expectTypeM position expectedType expr env
  parseTypecheckResultM typecheckResult

expectTypeM :: Typegetter a => BNFC'Position -> RawType -> a -> Env -> Either TypecheckingException ()
expectTypeM position expectedType expr env =
  runExcept $ runReaderT (TGU.expectTypeOrThrowM position expectedType expr) env


expectSymbolTypeOrThrowM :: BNFC'Position -> RawType -> Ident -> TypecheckerM
expectSymbolTypeOrThrowM position expectedType name = do
  env <- get
  case getType env name of
    Just rawType -> assertTypeOrThrowM expectedType rawType (InvalidTypeException position expectedType rawType)
    Nothing -> throwError $ UndefinedSymbolException position name


parseReturnTypecheckResultM :: BNFC'Position -> RawType -> Either TypecheckingException () -> TypecheckerM
parseReturnTypecheckResultM _ _ (Right _) = pure ()
parseReturnTypecheckResultM position expectedType (Left exception) =
  throwError exception

parseTypecheckResultM :: Either TypecheckingException () -> TypecheckerM
parseTypecheckResultM (Right _) = pure ()
parseTypecheckResultM (Left exception) =
  throwError exception


-- expectValidInitsNamesOrThrowM :: BNFC'Position -> [Init] -> TypecheckerM
-- expectValidInitsNamesOrThrowM position inits = do
--   let areNamesValid = CU.validateInitNames inits
--   assertOrThrowM areNamesValid (NamesDuplicationException position)


expectValidFunctionArgumentsOrThrowM :: BNFC'Position -> [Arg] -> TypecheckerM
expectValidFunctionArgumentsOrThrowM position arguments = do
  let areArgumentsValid = validateFunctionArguments arguments
  assertOrThrowM areArgumentsValid (FunctionArgumentsNameDuplicationException position arguments)


assertTypeOrThrowM :: Eq a => a -> a -> TypecheckingException -> TypecheckerM
assertTypeOrThrowM expectedType actualType = assertOrThrowM isValidType
  where
    isValidType = expectedType == actualType

assertOrThrowM :: Bool -> TypecheckingException -> TypecheckerM
assertOrThrowM True _          = pure ()
assertOrThrowM False exception = throwError exception
