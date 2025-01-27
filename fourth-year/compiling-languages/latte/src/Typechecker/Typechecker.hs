{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Typechecker.Typechecker
  ( typeCheck
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Grammar.Abs
import           Typechecker.Environment

import Typechecker.Monad
    ( EmptyTypegetterMonad,
      Typechecker(..),
      TypecheckerMonad,
      TypecheckerMonad',
      Typegetter(..),
      TypegetterMonad',
      getMethods, getFields )
import           Typechecker.Utils

import           Common.Exception
import           Common.RTypes
import qualified Data.Map as Map
import Data.List (find, group, sort)
import Lens.Micro
import Lens.Micro.Extras
import Data.Maybe

typeCheck :: Program -> Either StaticException ()
typeCheck program = runExcept $ evalStateT (checkTypeM Nothing program) emptyEnv

instance Typechecker Program where
  checkTypeM _ (Program _ topdef) = do
    let classTopDefs = filter isClassDef topdef
    let extClassTopDefs = filter isClassExtDef topdef
    let fnTopDefs = filter isFnDef topdef
    mapM_ collectSignature classTopDefs
    mapM_ collectSignature extClassTopDefs
    mapM_ collectSignature fnTopDefs
    classNames <- gets $ Map.keys . (^. classFields)
    let duplicates = findDuplicates classNames 
    when (not $ null duplicates) (throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates) BNFC'NoPosition)
    when ( (Ident "self" ) `elem` classNames) (throwError $ Exception (SymbolAlreadyDefinedException (Ident "self")) BNFC'NoPosition)
    get >>= assertMain
    mapM_ (checkTypeM Nothing) topdef
    where 
      isClassDef (TopClassDef _ (ClassDef _ _ _)) = True
      isClassDef _ = False
      isClassExtDef (TopClassDef _ (ClassExtDef _ _ _ _)) = True
      isClassExtDef _ = False
      isFnDef (TopClassFnDef _ _) = True
      isFnDef _ = False


findDuplicates :: (Ord a) => [a] -> [a]
findDuplicates xs = [x | (x:_:_) <- group (sort xs)]
collectSignature :: TopDef -> TypecheckerMonad
-- collectSignature (FnDef pos ret name args _) = do
collectSignature (TopClassFnDef _ (FnDef pos ret name args _)) = do
  let functionType = fromFunction ret args
  env <- get
  assertUniueArgs pos args
  assertNotVoidArgs pos args
  assertVarNotExists pos name
  -- print name 
  -- am i sure that it checks for duplicates?
  put $ updateType env name functionType

collectSignature (TopClassDef _ (ClassDef _ id  param)) = do 
  env <- get
  when (elem id (Map.keys $ env ^. classFields)) $ throwError $ Exception (SymbolAlreadyDefinedException id) BNFC'NoPosition
  collectFields id param
  collectMethods id param

collectSignature (TopClassDef _ (ClassExtDef _ id ext param )) = do 

  env <- get
  when (elem id (Map.keys $ env ^. classFields)) $ throwError $ Exception (SymbolAlreadyDefinedException id) BNFC'NoPosition
  collectFieldsExt id ext param 
  collectMethodsExt id ext param

selfId :: Ident
selfId = Ident "self"

withSelf :: [Ident] -> [Ident]
withSelf ids = selfId : ids

collectFieldsExt :: Ident -> Ident -> [ClassMember] -> TypecheckerMonad 
collectFieldsExt id' ext params = do 
  let fields = map  (\(ClassField _ typ name) -> (name, fromType typ)) (filter isField params ) 
  let field_names = map (\(name, _) -> name) fields
  let duplicates' = findDuplicates $ withSelf field_names
  unless (null duplicates') $ throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates') BNFC'NoPosition
  mapM_ assertValidClassType $ (filter isField params)
  env <- get
  let extId = Map.lookup ext (env ^. classFields)
  -- liftIO $ print $ " looking up " ++ show ext

  case extId of 
    Just _ -> do 
      let fields' = fromJust $ Map.lookup ext (env ^. classFields)
      let fieldsNames = map fst $ fields ++ fields' 
      let duplicates = findDuplicates $ withSelf fieldsNames 
      unless (null duplicates) $ throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates) BNFC'NoPosition
      modify $ classFields %~ Map.insert id' ( fields' ++ fields)
      modify $ extMap %~ Map.insert id' ext
    Nothing -> do 
      throwError $  Exception (NoSuchClassException ext) BNFC'NoPosition
  where 
    isField :: ClassMember -> Bool
    isField (ClassField _ _ _) = True 
    isField _ = False
collectFields :: Ident -> [ClassMember] -> TypecheckerMonad
collectFields id params = do 
  let fields = map  (\(ClassField _ typ name) -> (name, fromType typ)) (filter isField params ) 
  let field_names = map (\(name, _) -> name) fields
  let duplicates = findDuplicates $ withSelf field_names
  unless (null duplicates) $ throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates) BNFC'NoPosition
  mapM_ assertValidClassType $ (filter isField params)
  modify $ classFields %~ Map.insert id fields
  where 
    isField :: ClassMember -> Bool
    isField (ClassField _ _ _) = True 
    isField _ = False

collectMethodsExt :: Ident -> Ident -> [ClassMember] -> TypecheckerMonad
collectMethodsExt id' ext params = do 
  let methods = map  (\(ClassMethod _ f) -> getNameTypes f ) (filter isMethod params ) 
  let duplicates = findDuplicates $withSelf $  map fst methods
  unless (null duplicates) $ throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates) BNFC'NoPosition
  
  env <- get
  let methods' = fromJust $ Map.lookup ext (env ^. classMethods)
  let methods'' = methods' ++ methods
  let allMethodsArgTypes = concat $ map getArgTypes $ map snd methods''
  when (elem (RTClass selfId) allMethodsArgTypes) $ throwError $ Exception (SymbolAlreadyDefinedException selfId) BNFC'NoPosition
  when (elem selfId $ map fst methods'') $ throwError $ Exception (SymbolAlreadyDefinedException selfId) BNFC'NoPosition
  let nameDuplicates' = findDuplicates $ map fst methods''
  let duplicates'' = findDuplicates $ withSelf $ map fst methods''
  unless ( getWrongDuplicates methods methods' == []) $ throwError $ Exception (SymbolAlreadyDefinedException $ head $ getWrongDuplicates methods methods') BNFC'NoPosition
  modify $ classMethods %~ Map.insert id' methods''
  where 
    isMethod :: ClassMember -> Bool
    isMethod (ClassMethod {}) = True
    isMethod _ = False
    getNameTypes :: FnDef -> (Ident, RawType)
    getNameTypes (FnDef _ ret name args _) = (name, fromFunction ret args)
    getWrongDuplicates :: [(Ident, RawType)] -> [(Ident, RawType)] -> [Ident]
    getWrongDuplicates [] _ = []
    getWrongDuplicates ((name, typ):xs) ys = 
      case lookup name ys of 
        Just typ' -> if typ == typ' then getWrongDuplicates xs ys else [name]
        Nothing -> getWrongDuplicates xs ys
getArgTypes :: RawType -> [RawType]
getArgTypes (RTFun _ args) = args
getArgTypes _ = error "This should not happen"

collectMethods :: Ident -> [ClassMember] -> TypecheckerMonad 
collectMethods id params = do 
  let methods = map  (\(ClassMethod _ f) -> getNameTypes f ) (filter isMethod params ) 
  let duplicates = findDuplicates $ withSelf $ map fst methods
  unless (null duplicates) $ throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates) BNFC'NoPosition

  let allMethodsArgNames= concat $ map getArgNames $ filter isMethod params
  when (elem selfId allMethodsArgNames) $ throwError $ Exception (SymbolAlreadyDefinedException selfId) BNFC'NoPosition
  when (elem selfId $ map fst methods) $ throwError $ Exception (SymbolAlreadyDefinedException selfId) BNFC'NoPosition
  modify $ classMethods %~ Map.insert id methods
  where 
    isMethod :: ClassMember -> Bool
    isMethod (ClassMethod {}) = True 
    isMethod _ = False
    getNameTypes :: FnDef -> (Ident, RawType)
    getNameTypes (FnDef _ ret name args _) = (name, fromFunction ret args)
    getArgNames (ClassMethod _ (FnDef _ _ _ args _)) = map (\(Arg _ _ name) -> name) args
    getArgTypes _ = error "This should not happen"
instance Typechecker TopDef where
  checkTypeM :: Maybe RawType -> TopDef -> TypecheckerMonad
  checkTypeM _ ( TopClassFnDef _ (FnDef pos ret _ args block) )= do 
    envWithFunction <- get
    put $ updateTypes envWithFunction $ getArgumentsWithTypes args
    -- assert args are unique
    assertUniueArgs pos args
    -- assert types exist 
    let types = map (\(Arg _ typ _) -> fromType typ) args
    mapM_ (assertTypeExists pos) types
    env <- get
    let isClass = env ^. currentClass
    case isClass of 
      Just id -> do 
        let fields = fromJust $ Map.lookup id (env ^. classFields)
        putFields fields
        -- insert self as class type 
        put $ updateType env (Ident "self") (RTClass id)
      Nothing -> pure ()
    checkTypeM (Just (fromType ret)) block
    blockEnv <- get
    unless (hasReturnStatementOccured blockEnv || fromType ret == RTVoid)
      $ throwError (Exception NoReturnStatementException pos)
    put envWithFunction
    where 
      putFields :: [(Ident, RawType)] -> TypecheckerMonad
      putFields fields = do 
        env <- get
        -- assert fields are not in the map already 
        let ids = map fst fields
        let definedVars = map fst $ Map.toList $ env ^. types
        let duplicates = filter (\x -> x `elem` definedVars) ids
        unless (null duplicates) $ throwError $ Exception (SymbolAlreadyDefinedException $ head duplicates) pos
        put $ updateTypes env fields
  checkTypeM _ (TopClassDef _ (ClassDef pos id  members)) = do 
    assertFieldTypesExist pos id 
    methods <- membersToFuncs id members
    modify $ currentClass .~ Just id 
    mapM_ (checkTypeM Nothing) methods
    modify $ currentClass .~ Nothing
  checkTypeM _ (TopClassDef _ (ClassExtDef pos id _ members)) = do 
    assertFieldTypesExist pos id 
    methods <- membersToFuncs id  members
    modify $ currentClass .~ Just id 
    mapM_ (checkTypeM Nothing) methods
    modify $ currentClass .~ Nothing



membersToFuncs :: Ident -> [ClassMember] -> TypecheckerMonad' [TopDef]
membersToFuncs id members = do 
  let members' = (filter isMethod members)
  let methods = map typeToFnDef members'
  pure methods
  where 
    isMethod :: ClassMember -> Bool
    isMethod (ClassMethod {}) = True
    isMethod _ = False
    noPos = BNFC'NoPosition    
    typeToFnDef :: ClassMember -> TopDef
    typeToFnDef (ClassMethod _ def) = TopClassFnDef noPos def
    typeToFnDef _ = error "This should not happen"

instance Typechecker Block where
  checkTypeM expectedReturnType (Block _ statements) = do
    mapM_ (checkTypeM expectedReturnType) statements

-- instance Typechecker DeclKind where
--   checkTypeM _ (FDecl position name arguments returnType block) = do
--     unless
--       (areUniqueArgs arguments)
--       (throwError $ Exception (ArgDuplicateException arguments) position)
--     env <- get
--     put $ updateType env name $ fromFunction arguments returnType
--     envWithFunction <- get
--     put $ updateTypes envWithFunction $ getArgumentsWithTypes arguments
--     checkTypeM (Just (fromType returnType)) block
--     blockEnv <- get
--     unless (hasReturnStatementOccured blockEnv)
--       $ throwError (Exception NoReturnStatementException position)
--     put envWithFunction

--   checkTypeM _ (VDecl position name exprType item) = do
--     env <- get
--     case item of
--       (Init _ expr) -> do
--         case runExpr env expr of
--           Left exception -> throwError exception
--           Right etype    -> assertTypeC position etype rawExprType
--       _ -> pure ()
--     put $ updateType env name rawExprType
--     where
--       rawExprType = fromType exprType

declareVar:: Type -> Item -> TypecheckerMonad
declareVar typ (NoInit pos name) = do
  env <- get
  assertVarNotExists pos name 
  put $ updateType env name (fromType typ)
declareVar typ (Init pos name expr) = do
  env <- get
  case runExpr env expr of
    Left exception -> throwError exception
    Right etype    -> assertTypeC pos etype rawExprType
  put $ updateType env name rawExprType
  where
    rawExprType = fromType typ

instance Typechecker Stmt where
  checkTypeM _ (Empty _) = pure ()

  checkTypeM expectedReturnType (BStmt _ block) = do
    env <- get
    checkTypeM expectedReturnType block
    env' <- get
    let retval = hasReturnStatementOccured env'
    put $ setRetunFlag env retval

  checkTypeM _ (Decl pos typ items) = do
    assertDeclType pos typ 
    mapM_ (declareVar typ) items

  checkTypeM _ (Ass position nameExpr expr) = do
    env <- get

    case (runExpr env nameExpr, runExpr env expr) of
      (Left expcetion, _) -> throwError expcetion
      (_, Left exception) -> throwError exception
      (Right vtype, Right etype) -> assertTypeC position etype vtype

  checkTypeM _ (Incr position nameExpr) = do
    env <-get 
    let nameType = runExpr env nameExpr 
    case nameType of
      Right t -> assertTypeC position t RTInt
      Left exception -> throwError exception
    -- assertVarType position name RTInt

  checkTypeM _ (Decr position nameExpr) = do
    env <-get 
    let nameType = runExpr env nameExpr 
    case nameType of
      Right t -> assertTypeC position t RTInt
      Left exception -> throwError exception

  checkTypeM (Just expectedReturnType) (Ret position returnExpr) = do
    when (expectedReturnType == RTVoid) $ throwError $ Exception  VoidExprException position
    env <- get
    case runExpr env returnExpr of
      Right etype    -> assertTypeC position etype expectedReturnType
      Left exception -> throwError exception
    put $ returnStatementOccured env

  checkTypeM Nothing (Ret position _) =
    throwError $ Exception ReturnOutOfScopeException position

  checkTypeM (Just expectedReturnType) (VRet position) = do
    env <- get
    unless (expectedReturnType == RTVoid)
      $ throwError
          (Exception (InvalidReturnTypeException expectedReturnType) position)
    put $ returnStatementOccured env

  checkTypeM Nothing (VRet position) =
    throwError $ Exception ReturnOutOfScopeException position

  checkTypeM expectedReturnType (Cond position condition trueBlock) = do
    assertNotDecl trueBlock
    env <- get
    case runExpr env condition of
      Right etype    -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType trueBlock
    env' <- get
    let retval = hasReturnStatementOccured env'
    case isTrue condition of
      Just True  -> put $ setRetunFlag env retval
      _         -> put $ env

  checkTypeM expectedReturnType (CondElse position cond trueBlock falseBlock) = do
    assertNotDecl trueBlock
    assertNotDecl falseBlock
    env <- get
    case runExpr env cond of
      Right etype    -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType trueBlock
    env' <- get
    let retval' = hasReturnStatementOccured env'
    put $ env
    checkTypeM expectedReturnType falseBlock
    env'' <- get
    let retval'' = hasReturnStatementOccured env''
    case isTrue cond of
      Just True  -> put $ setRetunFlag env retval'
      Just False -> put $ setRetunFlag env retval''
      _         -> put $ setRetunFlag env (retval' && retval'')

  checkTypeM expectedReturnType (While position cond block) = do
    assertNotDecl block
    env <- get
    let retval = hasReturnStatementOccured env
    case runExpr env cond of
      Right etype    -> assertTypeC position etype RTBool
      Left exception -> throwError exception
    checkTypeM expectedReturnType block
    unless (isTrue cond == Just True)
      $ put $ setRetunFlag env retval

  checkTypeM _ (SExp _ expr) = do
    env <- get
    case runExpr env expr of
      Right _ -> pure ()
      Left exception -> throwError exception
  
  checkTypeM retTyp (For position typ id expr block) = do
    env <- get
    case runExpr env expr of
      Right t -> assertTypeC position t $ RTArr ( fromType typ ) 
      Left exception -> throwError exception
    declareVar typ (NoInit position id)
    checkTypeM retTyp block
    modify $ \env' -> env' & types %~ Map.delete id


instance Typegetter Expr where
  getTypeM (ELitInt pos val) = assertIntRange pos val
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
  getTypeM (EAdd position expr1 op expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    case op of 
      (Plus _) -> do
        if t1 == RTString && t2 == RTString
          then pure RTString
          else do
          assertTypeG position t1 RTInt
          assertTypeG position t2 RTInt
          pure RTInt
      (Minus _) -> do
        assertTypeG position t1 RTInt
        assertTypeG position t2 RTInt
        pure RTInt

  getTypeM (ERel position expr1 op expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    assertNotFunctionG position t1
    case op of 
      (EQU _) -> do
        -- no function 
        
        assertTypeG position t1 t2
        pure RTBool
      (NE _) -> do
        assertTypeG position t1 t2
        pure RTBool
      _ -> do
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
      Right t        -> do 
        assertTypeExists' position t
        pure t
      Left exception -> throwError exception
  getTypeM (EApp position name arguments) = do
    args <- mapM getTypeM arguments
    f <- getTypeM (EVar position name)
    case f of
      RTFun  returnType argumentsTypes-> do
        unless (length argumentsTypes == length args) $ throwError $ Exception (InvalidFunctionArgumentsTypesException argumentsTypes args) position
        compareArgs position argumentsTypes args
        pure returnType
      v -> throwError $ Exception (ExpectedFunctionException v) position
  getTypeM (ENewObject  pos typ) = do 
    assertTypeExists' pos $ fromType typ
    pure $ fromType  typ
  getTypeM (EMemberCall pos classExpr id exprs ) =  do 
    args <- mapM getTypeM exprs 
    blockType <- getTypeM classExpr 
    members <- getMethods blockType pos
    let fType = find (\m -> getMemberName m == id) members
    case (blockType, fType) of 
      (RTClass blockType, Just (ident, (RTFun returnType argumentsTypes))) -> do 
        compareArgs pos argumentsTypes args
        pure returnType
      _ -> throwError $ Exception (ExpectedFunctionException $ getType fType) pos
    where
      getMemberName (name, _) = name
      getType (Just (_, t)) = t
      getType Nothing = RTVoid
      -- compareArgs ::
      --      RawType -> [RawType] -> [RawType] -> EmptyTypegetterMonad
      -- compareArgs blockType expectedArgs actualArgs = do
      --   if  expectedArgs == actualArgs
      --     then pure ()
      --     else throwError
      --            $ Exception
      --                (InvalidFunctionArgumentsTypesException
      --                   expectedArgs
      --                   actualArgs)
      --                (getPos blockExpr)

  getTypeM (EMember pos classExpr id) = do 
    classType <- getTypeM classExpr
    case (classType, id) of 
      (RTArr _  , Ident "length") -> pure RTInt
      _ -> do 
        members <- getFields classType pos 
        case lookup id members of 
          Just t -> pure t
          Nothing -> throwError $ Exception (UndefinedSymbolException id) pos
  
  getTypeM (EArrGet pos expr1 expr2) = do
    t1 <- getTypeM expr1
    t2 <- getTypeM expr2
    elemType <- assertGetArrTypeG pos t1 
    assertTypeG pos t2 RTInt
    pure $ elemType
  
  getTypeM (ENewArray pos typ expr) = do
    t <- getTypeM expr
    assertTypeG pos t RTInt
    pure $ RTArr (fromType typ) 
    
        


    
  

compareArgs ::
      BNFC'Position -> [RawType] -> [RawType] -> EmptyTypegetterMonad
compareArgs pos expectedArgs actualArgs = do
  res <- mapM (compareArg) $ zip  expectedArgs actualArgs
  if  and res
    then pure ()
    else throwError
            $ Exception
                (InvalidFunctionArgumentsTypesException
                  expectedArgs
                  actualArgs)
                pos
  where 
    compareArg :: (RawType, RawType) -> TypegetterMonad' Bool
    compareArg (RTClass id, RTClass id') = do 
      env <- ask 
      let expId = Map.lookup id (env ^. extMap)
      
      pure $ id == id' || expId == Just id'
    compareArg (expected, actual) = pure $ expected == actual
  -- getTypeM (EApp position expr arguments) = do
  --   args <- mapM getTypeM arguments
  --   f <- getTypeM expr
  --   case f of
  --     RTFun argumentsTypes returnType -> do
  --       compareArgs position argumentsTypes args
  --       pure returnType
  --     v -> throwError $ Exception (ExpectedFunctionException v) position
  --   where
  --     compareArgs ::
  --          BNFC'Position -> [RawType] -> [RawType] -> EmptyTypegetterMonad
  --     compareArgs pos expectedArgs actualArgs = do
  --       if expectedArgs == [RTAny] || expectedArgs == actualArgs
  --         then pure ()
  --         else throwError
  --                $ Exception
  --                    (InvalidFunctionArgumentsTypesException
  --                       expectedArgs
  --                       actualArgs)
  --                    pos
  -- getTypeM (ELambda position arguments returnType block) = do
  --   unless
  --     (areUniqueArgs arguments)
  --     (throwError $ Exception (ArgDuplicateException arguments) position)
  --   local
  --     (`updateTypes` argumentsWithTypes)
  --     (runLocalCheckM arguments returnType block)
  --   where
  --     argumentsWithTypes = getArgumentsWithTypes arguments
  --     runLocalCheckM :: Typechecker a => [Arg] -> Type -> a -> TypegetterMonad
  --     runLocalCheckM args ret blck = do
  --       let rawReturnType = fromType ret
  --       let functionType = fromFunction args ret
  --       env <- ask
  --       let checkTypeResult =
  --             runExcept
  --               $ evalStateT (checkLambdaBodyM (Just rawReturnType) blck) env
  --       case checkTypeResult of
  --         Right _        -> pure ()
  --         Left exception -> throwError exception
  --       pure functionType
  --     checkLambdaBodyM ::
  --          Typechecker a => Maybe RawType -> a -> TypecheckerMonad
  --     checkLambdaBodyM expectedType blck = do
  --       checkTypeM expectedType blck
  --       blockEnv <- get
  --       unless
  --         (hasReturnStatementOccured blockEnv)
  --         (throwError $ Exception NoReturnStatementException position)
  -- getTypeM (EApp position name args) = getTypeM (CApp position (EVar position name) args)
