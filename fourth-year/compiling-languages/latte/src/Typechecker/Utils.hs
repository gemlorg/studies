{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}

module Typechecker.Utils where

import           Common.Exception
import           Common.RTypes
import           Common.GrammarUtils
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List             (nub, sort)
import           Grammar.Abs
import           Typechecker.Environment
import           Typechecker.Monad

import           Prelude
import Lens.Micro
import qualified Data.Map as Map
import Data.Maybe

getArgumentsWithTypes :: [Arg] -> [RVariable]
getArgumentsWithTypes = map getArgumentWithType

getArgumentWithType :: Arg -> RVariable
getArgumentWithType (Arg _ argType name)    = (name, fromType argType)

validateFunctionArguments :: [Arg] -> Bool
validateFunctionArguments arguments =
  numberOfArguments == numberOfUniqueArguments
  where
    argumentsNames = map getArgumentName arguments
    numberOfArguments = length argumentsNames
    numberOfUniqueArguments = length $ nub argumentsNames

getArgumentName :: Arg -> Ident
getArgumentName (Arg _ _ name)    = name

isFunctionType :: Type -> Bool
isFunctionType Fun {} = True
isFunctionType _      = False


assertTypeG :: BNFC'Position -> RawType -> RawType -> EmptyTypegetterMonad
assertTypeG pos t1 t2 = do
  ext <-extends t2 t1
  if t1 == t2 || ext
    then pure ()
    else throwError $ Exception (InvalidTypeException t2 t1) pos
  where extends (RTClass name) (RTClass name') = do
          env <- ask
          -- use map lookup 
          pure $ case Map.lookup name (env ^. extMap) of
            Just name'' -> name' == name''
            Nothing -> False
        extends _ _ = pure $ False


assertNotFunctionG :: BNFC'Position -> RawType -> EmptyTypegetterMonad
assertNotFunctionG pos t =
  when (isFunctionRawType t) $ throwError $ Exception (InvalidTypeException t RTVoid) pos

assertTypeC :: BNFC'Position -> RawType -> RawType -> TypecheckerMonad
assertTypeC pos t1 t2 = do
  ext <- extends t1 t2
  if t1 == t2 || ext
    then pure ()
    else do 
      throwError $ Exception (InvalidTypeException t2 t1) pos
    where extends (RTClass name) (RTClass name') = do
            env <- get
            -- use map lookup 
            case Map.lookup name (env ^. extMap) of
              Just name'' -> do    
                ext2 <- extends (RTClass name'') (RTClass name')
                pure $ name' == name''  || ext2
              Nothing -> pure False
          extends _ _ = (pure False)

assertVarExistsG :: BNFC'Position -> Ident -> TypegetterMonad
assertVarExistsG pos name = do
  env <- ask
  case getType env name of
    Just t  -> pure t
    Nothing -> throwError $ Exception (UndefinedSymbolException name) pos

assertDeclType:: BNFC'Position -> Type -> TypecheckerMonad
assertDeclType pos t = case fromType t of
  RTVoid -> throwError $ Exception InvalidDeclException pos
  RTFun _ _ -> throwError $ Exception InvalidDeclException pos
  _         -> assertTypeExists pos $ fromType t

assertMain :: Env -> TypecheckerMonad
assertMain env =
  let (name, typ) = mainSignature
  in case getType env name of
    Just t  -> if t == typ then
      pure () else
        throwError $ Exception (InvalidMainTypeException t) NoPos
    Nothing -> throwError $ Exception NoMainException NoPos

assertGetArrTypeG :: BNFC'Position -> RawType -> TypegetterMonad
assertGetArrTypeG pos (RTArr t ) = pure t
assertGetArrTypeG pos t = throwError $ Exception (NotArrayException t) pos
assertIntRange :: BNFC'Position -> Integer -> TypegetterMonad
assertIntRange pos i =
  if i >= minBound32 && i <= maxBound32
    then pure RTInt
    else throwError $ Exception (InvalidIntRangeException i) pos
  where
    minBound32 = 2^(31 :: Integer) * (-1)
    maxBound32 = 2^(31 :: Integer) - 1

isFunctionRawType :: RawType -> Bool
isFunctionRawType RTFun {} = True
isFunctionRawType _        = False

assertVarNotExists :: BNFC'Position -> Ident -> TypecheckerMonad
assertVarNotExists pos name = do
  env <- get
  case getType env name of
    Just _  -> throwError $ Exception (SymbolAlreadyDefinedException name) pos
    Nothing -> case env ^. currentClass of
      Just className -> do
        members <- getMembers' className pos
        if name `elem` map fst members
          then throwError $ Exception (SymbolAlreadyDefinedException name) pos
          else pure ()
      Nothing -> pure ()
    where getMembers' typ pos = do
            env <- get
            case Map.lookup typ (env ^. classFields) of
              Just members -> pure members
              Nothing -> throwError $ Exception (NoSuchClassException name) pos

assertUniueArgs :: BNFC'Position -> [Arg] -> TypecheckerMonad
assertUniueArgs pos args =
  if areUniqueArgs args
    then pure ()
    else throwError $ Exception (ArgDuplicateException args) pos

assertValidClassType ::  ClassMember -> TypecheckerMonad
assertValidClassType  (ClassField pos t _) = case fromType t of
  RTClass _ -> pure ()
  RTInt    -> pure ()
  RTBool  -> pure ()
  RTString -> pure ()
  RTArr _  -> pure ()
  _         -> throwError $ Exception (InvalidDeclException) pos

assertValidClassType _ = pure ()

assertFieldTypesExist :: BNFC'Position -> Ident -> TypecheckerMonad
assertFieldTypesExist pos id = do
  env <- get
  let fields = fromJust $ Map.lookup id (env ^. classFields)
  mapM_ (\(_, t) -> assertTypeExists pos t) fields

assertTypeExists :: BNFC'Position -> RawType -> TypecheckerMonad
assertTypeExists pos (RTClass name) = do
  env <- get
  case Map.lookup name (env ^. classFields) of
    Just _  -> pure ()
    Nothing -> throwError $ Exception (NoSuchClassException name) pos
assertTypeExists _ _ = pure ()


assertTypeExists' :: BNFC'Position -> RawType -> TypegetterMonad' ()
assertTypeExists' pos (RTClass name) = do
  env <- ask
  case Map.lookup name (env ^. classFields) of
    Just _  -> pure ()
    Nothing -> throwError $ Exception (NoSuchClassException name) pos
assertTypeExists' _ _ = pure ()

assertVarType :: BNFC'Position -> Ident -> RawType  -> TypecheckerMonad
assertVarType pos name t = do
  env <- get
  case getType env name of
    Just t' -> assertTypeC pos t t'
    Nothing -> throwError $ Exception (UndefinedSymbolException name) pos

assertNotVoidArgs :: BNFC'Position -> [Arg] -> TypecheckerMonad
assertNotVoidArgs _ = mapM_ assertNotVoid

assertNotVoid :: Arg -> TypecheckerMonad
assertNotVoid (Arg pos t name) =
  when (fromType t == RTVoid) $ throwError $ Exception (VoidArgumentException name) pos
getVarType :: BNFC'Position -> Ident -> Env -> Either StaticException RawType
getVarType position name env = do
  let currClass = env ^. currentClass
  case currClass of
    Just className -> do
      let fields = fromJust $ Map.lookup className (env ^. classFields)
      case lookup name fields of
        Just t -> pure t
        Nothing ->do
          -- error $ "looking up: " ++ show name ++ " in class " ++ show className ++ " fields: " ++ unwords (map (\(Ident name, t) -> name) fields)
          case getType env name of
            Just t  -> pure t
            Nothing -> throwError $ Exception (UndefinedSymbolException name) position
        --  throwError $ Exception (UndefinedSymbolException name) position
    Nothing -> do
      case getType env name of
        Just t  -> pure t
        Nothing -> throwError $ Exception (UndefinedSymbolException name) position

assertNotDecl :: Stmt -> TypecheckerMonad
assertNotDecl (Decl pos _ _) = throwError $ Exception NoBlockDeclException pos
assertNotDecl _              = pure ()
areUniqueArgs :: [Arg] -> Bool
areUniqueArgs arguments = do
  let sortedNames = sort $ map getArgumentName arguments
  all (uncurry (/=)) $ zip sortedNames (tail sortedNames)

runExpr :: Typegetter Expr => Env -> Expr -> Either StaticException RawType
runExpr env expr = runExcept $ runReaderT (getTypeM expr) env

isTrue :: Expr -> Maybe Bool
isTrue (ELitTrue _) = Just True
isTrue (ELitFalse _) = Just False
isTrue _            = Nothing