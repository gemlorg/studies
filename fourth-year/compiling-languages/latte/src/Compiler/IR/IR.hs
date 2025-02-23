
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}


{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE BangPatterns #-}

module Compiler.IR.IR where


import           Grammar.Abs

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map as Map
import Lens.Micro
import Common.Exception

import LLVM.AST.Type as AST hiding (Type)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.AST.Global hiding (name)
import LLVM.AST hiding (args, Type)
import qualified LLVM.AST.Instruction as I
import Common.GrammarUtils
import Compiler.IR.Monad
import Compiler.IR.Environment
import Common.RTypes
import Compiler.IR.Utils ( astFromArgs, astFromType, astFromIdent, predifinedDecl, astFromRType, arrIdent)
import LLVM.AST.Linkage
import LLVM.AST.AddrSpace
import LLVM.AST.Global
import Data.Maybe (fromJust)
import qualified LLVM.AST.Type
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Constant (Constant(Null))
import qualified LLVM.AST.Constant as Constant
import Data.ByteString(unpack)
import Compiler.Optimizer.Optimizer (nameToString, next_label, next_var_label)
import Data.List (find)
import Control.Exception (assert)



registerVars :: [Ident] -> IRMonad [(Ident, Name)]
registerVars [] = pure []
registerVars (i:is) = do
  env <- get
  let identLoc = fromJust $ Map.lookup i $ env ^. loc
  let v = fromJust $ Map.lookup identLoc $ env ^. store
  if isClassReference v then
    registerVars is
  else do
    newVarName <- getNextVarName
    v <- lookupIdent i
    let typ = toTypeV v
    modify $ insertIdent i (VReference newVarName typ)
    rest <- registerVars is
    pure $ (i, newVarName) : rest
    where
      isClassReference (VReference _ (RTClass _)) = True
      isClassReference _ = False

compareVars :: [Ident] -> Env -> Env -> IRMonad [Ident]
compareVars [] _ _ = pure []
compareVars (i:is) env1 env2 = do
  env'  <- get
  put env1
  v1 <- lookupIdent i
  put env2
  v2 <- lookupIdent i
  put env'
  if v1 == v2 then  ( compareVars is env1 env2) else do
    r <- compareVars is env1 env2
    pure $ i:r

zipVarsWhile :: [(Ident, Name)] -> Env -> Env -> IRMonad ()
zipVarsWhile [] _ _ = pure ()
zipVarsWhile ((i, n):is) env1 env2 = do
  env'  <- get
  put env1
  v1 <- lookupIdent i
  put env2
  v2 <- lookupIdent i
  put env'
  let b1 = env1 ^. currentBlockName
  let b2 = env2 ^. currentBlockName
  let typ = toTypeV v1
  unless ((v1 == v2)  || isClass typ || isArray typ)$ do
    modify $ phiCall n typ [(vOp v1, b1), (vOp v2, b2)]
    updateIdent i (VReference n typ)
  zipVarsWhile is env1 env2

isClass :: RawType -> Bool
isClass (RTClass _) = True
isClass _ = False

isArray:: RawType -> Bool
isArray (RTArr _ ) = True
isArray _ = False


zipVars :: [Ident] -> Env -> Env ->  IRMonad()
zipVars [] _ _ = pure ()
zipVars (i:is) env1 env2= do
  let block1 = env1 ^. currentBlockName
  let block2 = env2 ^. currentBlockName
  env' <- get
  put env1
  v1 <- lookupIdent i
  put env2
  v2 <- lookupIdent i
  put env'
  let typ = toTypeV v1
  unless ((v1 == v2) || isClass typ || isArray typ) $ do
    newName <- getNextVarName
    modify $ phiCall newName typ [(vOp v1, block1), (vOp v2, block2)]
    updateIdent i ( VReference newName typ)
  zipVars is env1 env2
  where
    isClass (RTClass _) = True
    isClass _ = False
    isArray (RTArr _ ) = True
    isArray _ = False
toTypeV :: Value -> RawType
toTypeV (VInt _) = RTInt
toTypeV VTrue = RTBool
toTypeV VFalse = RTBool
toTypeV (VReference _ t) = t
collectSignature :: TopDef -> IRMonad ()

collectSignature (TopClassFnDef _ (FnDef _ ret ident args _)) = do
  let rawArgTypes = fromArgs args
  let rawRet = fromType ret
  let funType = RTFun rawRet rawArgTypes
  _ <- fullStoreVar ident funType
  pure ()
collectSignature (TopClassDef _ (ClassDef _ ident items)) = do
  putFields ident items
  putMethods ident items

collectSignature (TopClassDef _ (ClassExtDef _ id extId items)) = do
  putExtFields id extId items
  putExtMethods id extId items


putExtFields :: Ident -> Ident -> [ClassMember] -> IRMonad ()
putExtFields id extId items = do
  env <- get
  let origFields = fromJust $ Map.lookup ( astFromIdent extId) $ env ^. classFields
  let fields = filter isField items
  let fields' = map (\(ClassField _ t i) -> (astFromIdent i, fromType t)) fields
  let fields_with_index =   map (\((a, b), i) -> (a, b, i)) $ zip fields' [length origFields + 1..]
  modify $  classFields %~ \cf -> Map.insert (astFromIdent id) (origFields ++ fields_with_index) cf
  where
        isField (ClassField _ _ _) = True
        isField _ = False

putExtMethods :: Ident -> Ident -> [ClassMember] -> IRMonad ()
putExtMethods id extId items = do
  env <- get
  let origMethods = fromJust $ Map.lookup ( astFromIdent extId) $ env ^. classMethods
  let methods = filter isMethod items
  let classParameter = RTClass id
  let methods' = map (\(ClassMethod _ (FnDef _ ret i args _) ) -> (i, astFromFIdent id i, fromType ret, classParameter : (getArgTypes args) )) methods
  let methods_with_index =   map (\((a, a', b, c), i) -> (a, a', b, c, i)) $ zip methods' [0..]
  let finalMethods = combineMethods origMethods methods_with_index id extId
  modify $ classMethods %~ Map.insert (astFromIdent id) finalMethods
  where isMethod (ClassMethod _ _) = True
        isMethod _ = False
        getArgTypes args = map (\(Arg _ t _) -> fromType t) args

combineMethods :: [ClassMethod] -> [ClassMethod] -> Ident -> Ident -> [ClassMethod]
combineMethods orig new id extId = do
  let newNames = map (\(a, _, _, _, _) -> a) new
  let origNames = map (\(a, _, _, _, _) -> a) orig
  let replacedOrig = map (\(a, a', b, c, i) -> if elem a newNames  then fromJust $ find (\(a', _, _, _, _) -> a == a') new else (a, a', b, c, i)) orig
  let filteredNew = filter (\(a, _, _, _, _) -> not $ elem a origNames) new
  let totalNames = replacedOrig ++ filteredNew
  let indexedTotalNames = zip totalNames [0..]
  let finalNames = map (\((a,a', b, c, i), j) -> (a, a', b, c, j)) $ indexedTotalNames
  finalNames

loadNullClass :: Name-> Name -> IRMonad ()
loadNullClass block var = do
    let allocInst = var := Alloca
          { allocatedType = astFromRType (RTClass (Ident (nameToString block))) -- Type of %Counter*
          , numElements = Nothing
          , LLVM.AST.alignment = 0
          , LLVM.AST.metadata = []
          }

    let storeInst = Do $ Store
          { volatile = False
          , address = LocalReference (PointerType (astFromRType (RTClass (Ident (nameToString block)))) (AddrSpace 0)) var
          , value = ConstantOperand $ C.Null $ astFromRType (RTClass (Ident (nameToString block)))
          , maybeAtomicity = Nothing
          , LLVM.AST.alignment = 0
          , LLVM.AST.metadata = []
          }
    modify $ instrAcc %~ (++ [allocInst, storeInst])


loadFunction :: Name -> Name  -> Ident -> [Value] -> IRMonad (Name, [Value], RawType)
loadFunction v c f args  = do
  let classType = NamedTypeReference c
  let classPtr = PointerType classType (AddrSpace 0)
  let vtype = NamedTypeReference $ getClassVTypeName $ Ident $ nameToString c
  env <- get
  let methods = fromJust $ Map.lookup c $  env ^. classMethods
  let _ = forceList methods
  let !y = f
  let !y = c
  let (fid, fname, frtype, fargs, findex) = fromJust $ find (\(s,_,  _, _, _) -> s == f) $  methods
  let ftype = astFromRType $ RTFun frtype fargs
  v2 <- getNextVarName
  v3 <- getNextVarName
  v4 <- getNextVarName
  v5 <- getNextVarName
  let getElemInstr = v2 := GetElementPtr False (LocalReference classPtr v ) [iOp 0, iOp 0] []
  let loadInstr2 = v3 := Load False (LocalReference (myPointerType $ myPointerType vtype) v2) Nothing 0 []
  let getElemInstr2 = v4 := GetElementPtr False (LocalReference (myPointerType vtype) v3) [iOp 0, iOp (fromIntegral findex)] []
  let loadInstr3 = v5 := Load False (LocalReference (myPointerType $ myPointerType ftype) v4) Nothing 0 []
  bitCastInstrVars <- mapM  getBitCastInstr $ zip (VReference v (RTClass $ Ident $ nameToString c) : args) fargs
  let (bitCastInstrs, vars) = unzip bitCastInstrVars
  modify $ instrAcc %~ (++ [ getElemInstr, loadInstr2, getElemInstr2, loadInstr3] ++ concat bitCastInstrs)
  pure (v5, vars, RTFun frtype fargs)


forceList :: [a] -> ()
forceList []     = ()
forceList (x:xs) = let !y = x in forceList xs

getElemInit:: Ident -> Name -> (Name , RawType, Int) -> IRMonad [Named Instruction]
getElemInit classId var (id, typ, i) = do
  let classType = NamedTypeReference $ astFromIdent classId
  let varType = astFromRType typ
  let classPtr = PointerType classType (AddrSpace 0)
  defaultValue <- case typ of
        RTInt -> pure $ astIntOp 0
        RTBool -> pure $ astBoolOp False
        RTString -> do 
          var <- getNextVarName
          op <- defineEmptyString
          modify $ stringCastCall var op
          pure $ vOp $ VReference var RTString
        RTClass id -> pure $ ConstantOperand $ C.Null $ myPointerType $ (NamedTypeReference $ astFromIdent id)
        RTArr typ  -> pure $ ConstantOperand $ C.Null $ myPointerType $ (NamedTypeReference $ astFromIdent Compiler.IR.Utils.arrIdent)
        _ -> error "this type isn't supposed to have default value"
  v1 <- getNextVarName
  let getElemInstr = v1 := GetElementPtr False (LocalReference ( classPtr) var ) [iOp 0, iOp (fromIntegral i)] []
  let storeInstr2 = Do $ Store {
    volatile = False,
    address = LocalReference (myPointerType  (varType)) v1,
    value = defaultValue,
    maybeAtomicity = Nothing,
    LLVM.AST.alignment = 0,
    LLVM.AST.metadata = []
  }


  pure [getElemInstr, storeInstr2]


getNextGlobalName :: IRMonad Name
getNextGlobalName = do
  env <- get
  let name = env ^. cGlobalId
  modify $ cGlobalId %~ (+1)
  pure $ mkName $ "_g" ++ show name
addGlobalString :: String -> IRMonad Operand
addGlobalString s = do
  var <- getNextGlobalName
  let def =  GlobalDefinition globalVariableDefaults {
    name = var,
    isConstant = True,
    initializer = Just $ C.Array AST.i8 $ map (C.Int 8 . fromIntegral . fromEnum) (s ++ "\0"),
    LLVM.AST.Global.type' = ArrayType (fromIntegral $ length s + 1) i8
  }
  modify $ \env -> env & globalDefs %~ (++ [def])
  let globalType = PointerType (ArrayType (fromIntegral $ length s + 1) i8) (AddrSpace 0)
  pure $ ConstantOperand $ C.GlobalReference globalType var

defineEmptyString :: IRMonad Operand
defineEmptyString = do
  env <- get
  case env ^. emptyStringName of
    Just op -> pure op
    Nothing -> do
      op' <- addGlobalString ""
      modify $ \env -> env & emptyStringName ?~ op'
      pure op'

initVar :: Ident -> RawType -> IRMonad ()
initVar ident typ = do
  case typ of
    RTInt -> modify $ insertIdent ident (VInt 0)
    RTBool -> modify $ insertIdent ident VFalse
    RTString -> do
      var <- getNextVarName
      op <- defineEmptyString
      modify $ stringCastCall var op
      modify $ insertIdent ident $ VReference var RTString
    RTClass id -> do
      newName <- getNextVarName
      loadNullClass  ( astFromIdent id ) newName

      modify $ insertIdent ident $ VReference newName (RTClass id)
    RTArr typ   -> do
      modify $ insertIdent ident $ VReference (mkName "null") (RTArr typ) 
    _ -> error "this type isn't supposed to have default value"
  pure ()


declareVar :: Type -> Item -> IRMonad [BasicBlock]
declareVar typ (NoInit pos ident) = do
  _ <- initVar ident (fromType typ)
  pure []

declareVar _ (Init _ ident expr) = do
  exprRes <- irExprM expr
  case exprRes of
    VReference n (RTClass cId) -> do
      newName <- getNextVarName
      allocClass cId newName
      modify $ instrAcc %~ (++ [Do $ Store False (LocalReference (myPointerType (astFromRType (RTClass cId)) ) newName) (vOp exprRes) Nothing 0 []])
      modify $ insertIdent ident $ VReference newName (RTClass cId)
    _ ->
      modify $ insertIdent ident exprRes
  pure []

allocClass :: Ident -> Name -> IRMonad ()
allocClass cId var = do
  let classType = NamedTypeReference $ astFromIdent cId
  let classPtr = PointerType classType (AddrSpace 0)
  modify $ instrAcc %~ (++ [var := Alloca (classPtr) Nothing 0 []])
  modify $ instrAcc %~ (++ [Do $ Store False (LocalReference (myPointerType classPtr) var) (ConstantOperand $ C.Null classPtr) Nothing 0 []])






irExprM :: Expr -> IRMonad Value
irExprM (EVar _ ident) = lookupIdent ident
irExprM (ELitInt _  i) = pure $ VInt i

irExprM (EString _ s) = do
  str <- addGlobalString s
  var <- getNextVarName
  modify $ stringCastCall var str
  pure $ VReference var RTString

irExprM (ELitTrue _) = pure VTrue

irExprM (ELitFalse _) = pure VFalse

irExprM (EApp _ ident exprs) = do
  argNames <- mapM irExprM exprs
  let args = map vOp argNames
  name <- lookupIdent ident
  var <- getNextVarName
  funcCall var ident args
  pure $ VReference var $ getReturnTypeV name
    where getReturnTypeV (VReference _ (RTFun ret _)) = ret
          getReturnTypeV _ = error "this should be a function"
irExprM (Neg _ expr) = do
  exprRes <- irExprM expr
  case exprRes of
    VInt i -> pure $ VInt (-i)
    _ -> do
      var <- getNextVarName
      modify $ binOpCall var RTInt (Sub False False)  (astIntOp 0) (vOp exprRes)
      pure $ VReference var RTInt

irExprM (Not _ expr) = do
  exprRes <- irExprM expr
  case exprRes of
    VTrue -> pure VFalse
    VFalse -> pure VTrue
    _ -> do
      var <- getNextVarName
      modify $ binOpCall var RTBool Xor  (astBoolOp True) (vOp exprRes )
      pure $ VReference var RTBool

irExprM (EAdd _ expr1 op expr2) = do
  expr1Res <- irExprM expr1
  expr2Res <- irExprM expr2
  case (expr1Res, expr2Res) of
    (VInt i1, VInt i2) -> pure $ VInt $ case op of
                            Plus _ -> i1 + i2
                            Minus _ -> i1 - i2
    (VReference _ RTString, VReference _ RTString) -> do
      var <- getNextVarName
      funcCall var (Ident "_concatStrings") [vOp expr1Res, vOp expr2Res]
      pure $ VReference var RTString
    _ -> do
      var <- getNextVarName
      let op' = case op of
            Plus _ -> Add False False
            Minus _ -> Sub False False
      modify $ binOpCall var RTInt op' (vOp expr1Res ) (vOp expr2Res )
      pure $ VReference var RTInt

irExprM (EMul _ expr1 op expr2) = do
  expr1Res <- irExprM expr1
  expr2Res <- irExprM expr2
  case (expr1Res, expr2Res) of
    (VInt i1, VInt i2) -> pure $ VInt $ case op of
                            Times _ -> i1 * i2
                            Div _ -> i1 `div` i2
                            Mod _ -> let r = i1 `mod` i2
                                     in if (i1 < 0) && (r /= 0)
                                        then r - abs i2
                                        else r
    _ -> do
      var <- getNextVarName
      let op' = case op of
            Times _ -> Mul False False
            Div _ -> SDiv False
            Mod _ -> SRem
      modify $ binOpCall var RTInt op' (vOp expr1Res) (vOp expr2Res)
      pure $ VReference var RTInt

irExprM (ERel _ expr1 op expr2) = do
  expr1Res <- irExprM expr1
  expr2Res <- irExprM expr2
  var <- getNextVarName
  let op' = case op of
        LTH _ -> I.ICmp P.SLT
        LE _ -> I.ICmp P.SLE
        GTH _ -> I.ICmp P.SGT
        GE _ -> I.ICmp P.SGE
        EQU _ -> I.ICmp P.EQ
        NE _ -> I.ICmp P.NE
  case (expr1Res, expr2Res) of
      (VReference _ RTString, VReference _ RTString) -> do
        case op of
          EQU _ -> do
            funcCall var (Ident "_compareStrings") [vOp expr1Res , vOp expr2Res ]
            newVar <- getNextVarName
            modify $ truncCall newVar RTBool (LocalReference (IntegerType 32) var)
            pure $ VReference newVar RTBool
          NE _ -> do
            funcCall var (Ident  "_compareStrings") [vOp expr1Res , vOp expr2Res ]
            newVar <- getNextVarName
            modify $ truncCall newVar RTBool (LocalReference (IntegerType 32) var)
            newVar2 <- getNextVarName
            modify $ binOpCall newVar2 RTBool Xor (LocalReference (IntegerType 1) newVar) (astBoolOp True)
            pure $ VReference newVar2 RTBool
          _ -> error "this should not happen"
      (VInt i1, VInt i2) -> do
          let res = case op of
                EQU _ -> i1 == i2
                NE _ -> i1 /= i2
                LTH _ -> i1 < i2
                LE _ -> i1 <= i2
                GTH _ -> i1 > i2
                GE _ -> i1 >= i2
          pure $ if res then VTrue else VFalse
      (VTrue, VTrue) -> cmpBool True True op
      (VTrue, VFalse) -> cmpBool True False op
      (VFalse, VTrue) -> cmpBool False True op
      (VFalse, VFalse) -> cmpBool False False op
      _ -> do
        modify $ binOpCall var RTBool op' (vOp expr1Res) (vOp expr2Res)
        pure $ VReference var  RTBool
    where
          cmpBool b1 b2 (EQU _ ) = pure $ if b1 == b2 then VTrue else VFalse
          cmpBool b1 b2 (NE _ ) = pure $ if b1 /= b2 then VTrue else VFalse
          cmpBool _ _ _ = error "this should not happen"
irExprM (EAnd _ expr1 expr2) = do
  expr1Res <- irExprM expr1
  case expr1Res of
    VFalse -> pure VFalse
    VTrue -> irExprM expr2
    _ -> do
      exprBlockName <- getNextBlockName
      afterBlockName <- getNextBlockName
      origJump <- gets (^. currentBlockName)
      finalizeBlock (condJump (vOp expr1Res) exprBlockName afterBlockName)
      modify $ putBlockName exprBlockName
      expr2Res <- irExprM expr2
      exprJump <- gets (^. currentBlockName)
      finalizeBlock (jump afterBlockName)
      modify $ putBlockName afterBlockName
      var <- getNextVarName
      modify $ phiCall var RTBool [(astBoolOp False, origJump), (vOp expr2Res, exprJump)]
      pure $ VReference var RTBool


irExprM (EOr _ expr1 expr2) = do
  expr1Res <- irExprM expr1
  case expr1Res of
    VTrue -> pure VTrue
    VFalse -> irExprM expr2
    _ -> do
      exprBlockName <- getNextBlockName
      afterBlockName <- getNextBlockName
      origJump <- gets (^. currentBlockName)
      finalizeBlock (condJump (vOp expr1Res) afterBlockName exprBlockName )
      modify $ putBlockName exprBlockName
      expr2Res <- irExprM expr2
      exprJump <- gets (^. currentBlockName)
      finalizeBlock (jump afterBlockName)
      modify $ putBlockName afterBlockName
      var <- getNextVarName
      modify $ phiCall var RTBool [(astBoolOp True,origJump), (vOp expr2Res, exprJump)]
      pure $ VReference var RTBool
irExprM (ENewObject _ typ) = do
  let className = getName typ
  initClass className 
    where
      getName  (Class _ i) = i
      getName  _ = error "this should not happen"


irExprM (EMemberCall _ expr ident exprs ) = do

  exprRes <- irExprM expr
  argNames <- mapM irExprM exprs

  case exprRes of
    VReference n(RTClass className) -> do

      (v, fargs, ftype) <- loadFunction n  (astFromIdent className)  ident argNames
      let args = map vOp fargs
      v' <- getNextVarName
      funcCallWType v' (Ident $ nameToString v) ftype  args
      pure $ VReference v' $ getReturnType ftype
    _ -> error "this isn't a class!"

irExprM (EMember _ expr ident) = do
  exprRes <- irExprM expr
  env <- get
  case exprRes of
    VReference n(RTClass className) -> do
      let !fields = fromJust $ Map.lookup (astFromIdent className)  $ env ^. classFields
      let (_, !typ, !i) = fromJust $ find (\(s, _, _) -> s == astFromIdent ident) fields
      v1 <- loadMemberReference exprRes  i
      v2 <- getNextVarName
      let loadInstr = v2 := Load False (LocalReference (myPointerType $ astFromRType typ) v1) Nothing 0 []
      modify $ instrAcc %~ (++ [loadInstr])
      pure $ VReference v2 typ
    VReference na (RTArr typ) -> do
        v1 <- loadMemberReference exprRes 1
        v2 <- getNextVarName
        let loadInstr = v2 := Load False (LocalReference (myPointerType $ astFromRType RTInt) v1) Nothing 0 []
        modify $ instrAcc %~ (++ [loadInstr])
        pure $ VReference v2 RTInt
          
    _ -> error "this isn't a class!"

irExprM (ENewArray _ typ expr) = do
  lenRes <- irExprM expr
  val <- initArray (fromType typ) lenRes
  pure $ VReference val (RTArr ( fromType typ))

irExprM (EArrGet _ expr1 expr2) = do
  expr1Res <- irExprM expr1
  expr2Res <- irExprM expr2
  getArrayIndex expr1Res expr2Res

initClass :: Ident -> IRMonad Value
initClass className = do
  v2 <- getNextVarName
  v3 <- getNextVarName

  let classType = astFromRType (RTClass className)
  let mallocInstr = v2 := Call Nothing CC.C [] (Right $ ConstantOperand $ C.GlobalReference (mallocType (NamedTypeReference $ astFromIdent className)) (getMallocName className)) [] [] []
  let getElemInstr = v3 := GetElementPtr False (LocalReference ( classType) v2) [iOp 0, iOp 0] []
  let storeInstr2 = Do $ Store {
    volatile = False,
    address = LocalReference (myPointerType $ myPointerType (NamedTypeReference (getClassVTypeName className) )) v3,
    value = ConstantOperand $ C.GlobalReference ((myPointerType( NamedTypeReference (getClassVTypeName className))))  (getClassVDataName className),
    maybeAtomicity = Nothing,
    LLVM.AST.alignment = 0,
    LLVM.AST.metadata = []
  }
  fields <- gets $ \env -> fromJust $ Map.lookup (astFromIdent className) $ env ^. classFields
  inits <- mapM (getElemInit className v2 ) fields
  modify $ instrAcc %~ (++ [mallocInstr, getElemInstr, storeInstr2] ++ concat inits )
  pure $ VReference v2 (RTClass className)

initArray :: RawType -> Value -> IRMonad Name
initArray typ len = do
  intLen <- rTypeToLength typ
  c <- getNextVarName
  v1 <- getNextVarName
  v2 <- getNextVarName
  v3 <- getNextVarName
  v4 <- getNextVarName
  let v2Val = VReference v2 RTString
  let v1Val = VReference v1 RTInt
  let v3t = myPointerType $ myPointerType $ i8
  let v4t = myPointerType $ i32
  let v3op = LocalReference v3t v3
  let v4op = LocalReference v4t v4
  let cop = LocalReference (astFromRType (RTClass arrIdent )) c

  modify $ instrAcc %~ (++ [c := Call Nothing CC.C [] (Right $ ConstantOperand $ C.GlobalReference (mallocType (NamedTypeReference $ astFromIdent arrIdent)) (getMallocName arrIdent)) [][][]])
  modify $ instrAcc %~ (++ [v1 := Mul False False (astIntOp intLen) (vOp len) [] ])
  modify $ instrAcc %~ (++ [v2 := Call Nothing CC.C [] (Right $ ConstantOperand $ C.GlobalReference (mallocType(IntegerType 8)  ) (Name "_malloc")) [(vOp v1Val, [])] [] [] ])
  -- store length in c 
  modify $ instrAcc %~ (++ [v3 := GetElementPtr False (cop) [iOp 0, iOp 0] []])
  modify $ instrAcc %~ (++ [Do $ Store False (v3op) (vOp v2Val) Nothing 0 []])
  modify $ instrAcc %~ (++ [v4 := GetElementPtr False (cop) [iOp 0, iOp 1] []])
  modify $ instrAcc %~ (++ [Do $ Store False (v4op) (vOp len) Nothing 0 []])
  pure c

getArrayIndex :: Value -> Value -> IRMonad Value
getArrayIndex arrVal@(VReference arrName (RTArr typ )) indexVal = do
  v2Val <- getArrayIndexPointer arrVal indexVal
  v3 <- getNextVarName 
  modify $ instrAcc %~ (++ [v3 := Load False v2Val Nothing 0 []])
  pure $ VReference v3 typ
getArrayIndex _ _ = error "this isn't an array!"

getArrayIndexPointer :: Value -> Value -> IRMonad Operand
getArrayIndexPointer arrVal@(VReference arrName (RTArr typ )) indexVal = do
  arrayPointer <- loadMemberReference arrVal 0
  v0 <- getNextVarName
  v1 <- getNextVarName
  v2 <- getNextVarName
  let arrClassName = astFromRType $ RTClass arrIdent
  let currArrType = myPointerType $ astFromRType RTString
  let arrType = myPointerType $ astFromRType typ
  modify $ instrAcc %~ (++ [v0 := Load False (LocalReference currArrType arrayPointer) Nothing 0 []])
  modify $ instrAcc %~ (++ [v1 := BitCast (LocalReference (astFromRType RTString) v0) (arrType) []])
  modify $ instrAcc %~ (++ [v2 := GetElementPtr False (LocalReference arrType v1) [vOp indexVal] []])
  pure (LocalReference (myPointerType $ astFromRType typ) v2)

getArrayIndexPointer _ _ = error "this isn't an array!"

rTypeToLength :: RawType -> IRMonad Integer
rTypeToLength (RTInt) = pure 4
rTypeToLength (RTBool) = pure 1
rTypeToLength (RTString) = pure 8
rTypeToLength (RTClass _) = pure 8
rTypeToLength (RTArr _) = error "multi-dimensional arrays are not supported"
rTypeToLength (RTFun _ _) = error "functions are not supported"
rTypeToLength (RTVoid) = error "void is not supported"

class IR a where
  irM ::  a -> IRMonad ()

instance IR Block where
  irM (Block _ stmts) = do
    origLoc <- gets (^. loc)
    mapM_ irM stmts
    modify $ \env' -> env' & loc .~ origLoc

instance IR Stmt where
  irM (Empty _) = pure ()
  irM (BStmt _ block) = irM block
  irM (Decl _ typ items) = mapM_ (declareVar typ) items
  irM (Ass pos ident expr) = do
    expRes <- irExprM expr
    updateExpr ident expRes
    pure ()
  irM (Incr _ ident) = do
    res <- irExprM (EAdd  NoPos ident (Plus NoPos) (ELitInt NoPos 1))
    updateExpr ident res
    pure ()
  irM (Decr _ ident) = do
    res <- irExprM (EAdd  NoPos ident (Minus NoPos) (ELitInt NoPos 1))
    updateExpr ident res
    pure ()
  irM (Grammar.Abs.VRet _ ) = do
    finalizeRetBlock vretCall
    modify $ \env -> env & returnFlag .~ True
  irM (Grammar.Abs.Ret _  expr) = do
    expRes <- irExprM expr
    ret <- retCall expRes
    finalizeRetBlock  ret
    modify $ \env -> env & returnFlag .~ True

  irM (Cond _ expr stmt) = do
    expRes <- irExprM expr
    case expRes of
      VTrue -> irM stmt
      VFalse -> pure ()
      _ -> do
        ifBlockName <- getNextBlockName
        afterBlockName <- getNextBlockName

        env <- get
        let retf = env ^. returnFlag
        let origVars = env ^. loc
        finalizeBlock (condJump (vOp expRes) ifBlockName afterBlockName)

        modify $ putBlockName ifBlockName
        irM stmt
        env' <- get
        finalizeBlock (jump afterBlockName)

        modify $ putBlockName afterBlockName
        modify $ returnFlag .~ retf
        modify $ loc .~ origVars
        let idents = Map.keys $ env ^. loc
        zipVars idents env env'

        pure ()

  irM (CondElse _ expr stmtTrue stmtFalse) = do
    expRes <- irExprM expr
    case expRes of
      VTrue -> irM stmtTrue
      VFalse -> irM stmtFalse
      _ -> do
        ifBlockName <- getNextBlockName
        elseBlockName <- getNextBlockName
        afterBlockName <- getNextBlockName

        env <- get
        let origVars = env ^. loc
        finalizeBlock (condJump (vOp expRes) ifBlockName elseBlockName)

        modify $ putBlockName ifBlockName
        irM stmtTrue
        env' <- get
        ret1 <- gets (^. returnFlag)
        finalizeBlock (jump afterBlockName)

        modify $ putBlockName elseBlockName
        modify $ returnFlag .~ env ^. returnFlag
        modify $ loc .~ origVars
        irM stmtFalse

        env'' <- get
        ret2 <- gets (^. returnFlag)
        finalizeBlock (jump afterBlockName)

        modify $ putBlockName afterBlockName
        modify $ returnFlag .~ (ret1 && ret2)
        modify $ loc .~ origVars
        zipVars (Map.keys $ env ^. loc) env' env''
        pure ()

  irM (While _ expr stmt) = do
    loopBlockName <- getNextBlockName
    condBlockName <- getNextBlockName
    afterBlockName <- getNextBlockName
    env <- get
    finalizeBlock (jump condBlockName)
    modify $ putBlockName loopBlockName
    envSave <- get
    _ <- registerVars $ Map.keys $ env ^. loc
    env' <- get
    irM stmt
    env'' <- get
    modifiedVars <-compareVars (Map.keys $ env ^. loc) env' env''
    put envSave
    registeredVars <- registerVars modifiedVars
    irM stmt
    env''' <- get
    finalizeBlock (jump condBlockName)
    modify $ putBlockName condBlockName
    modify $ \denv -> denv & returnFlag .~ env ^. returnFlag
    zipVarsWhile registeredVars env env'''
    expRes <- irExprM expr
    case expRes of
      VTrue -> finalizeBlock (jump loopBlockName)
      VFalse -> finalizeBlock (jump afterBlockName)
      _ -> finalizeBlock (condJump (vOp expRes) loopBlockName afterBlockName)
    modify $ putBlockName afterBlockName

-- for as while
-- for (typ elem: arr) stmt 
-- > let i = -1; while (i < arr.length - 1) { i++; elem = arr[i]; stmt }
  irM (For _ typ ident expr1 stmt) = do
    nextVar <- getNextVarName
    let var = Ident $ nameToString nextVar
    let decl = Decl NoPos typ [Init NoPos var (ELitInt NoPos (0))]
    let cond = ERel NoPos (EVar NoPos var) (LTH NoPos ) ((EMember NoPos  (expr1) ((Ident "length")) ))
    let addone = (Incr NoPos (EVar NoPos var))
    let initElem = Decl NoPos typ [Init NoPos ident (EArrGet NoPos expr1 (EVar NoPos var))]
    let stmt' = BStmt NoPos $ Block NoPos [initElem, stmt, addone]
    mapM_ irM [decl, While NoPos cond stmt']

  irM (SExp _ expr) = do
    _ <- irExprM expr
    pure ()


irTopDefM :: TopDef -> IRMonad [Definition]
irTopDefM (TopClassFnDef _ (FnDef _ ret ident args block)) = do
  modify $ putBlockName (mkName "entry")
  modify $ \env -> env & returnFlag .~ False
  modify $ \env -> env & pBlocks .~ []
  modify $ \env -> env & cVarId .~ 0 & cBlockId .~ 0

  modify $ \env -> env & currentReturnType .~ fromType ret
  loc' <- gets (^. loc)
  store' <- gets (^. store)
  modify $ insertArgs args
  mapM_ loadClassArgs args
  irM block
  modify $ \env -> env & loc .~ loc'
  modify $ \env -> env & store .~ store'
  env <- get
  when ((fromType ret == RTVoid) && env ^. currentBlockName /=
    mkName "wrong") $ finalizeRetBlock vretCall
  env' <- get
  let blocks = env' ^. pBlocks
  -- add block with just return statement if the function is void 
  let func' = GlobalDefinition $ functionDefaults {
    name = astFromIdent ident,
    parameters = (astFromArgs args, False),
    returnType = astFromType ret,
    basicBlocks = blocks
  }
  pure [func']

irTopDefM (TopClassDef _ (ClassDef _ ident items)) = do
  genClassType ident
  genVType ident
  genVData ident
  genClassMalloc ident

  genMethods ident items



irTopDefM (TopClassDef _ (ClassExtDef _ ident _ items)) = do
  genClassType ident
  genVType ident
  genVData ident
  genClassMalloc ident
  genMethods ident items

loadClassArgs :: Arg -> IRMonad ()
loadClassArgs (Arg _ (Class _ id) ident) = do

  varName <- getNextVarName
  let classType = NamedTypeReference $ astFromIdent id
  let classPtr = PointerType classType (AddrSpace 0)
  modify $ instrAcc %~ (++ [varName := Alloca (classPtr) Nothing 0 []])
  modify $ instrAcc %~ (++ [Do $ Store False (LocalReference (myPointerType classPtr) varName) (LocalReference classPtr $ astFromIdent ident) Nothing 0 []])
  modify $ insertIdent ident (VReference varName (RTClass id))
loadClassArgs _ = pure()


genMethods :: Ident -> [ClassMember] -> IRMonad [Definition]
genMethods id items = do
  let methods = map methodToFunc (filter isMethod items)

  defs <- mapM (irMethod id)  methods
  pure $ concat defs
  where
    methodToFunc (ClassMethod _ a) = a
    methodToFunc _ = error "this should not happen"
    isMethod (ClassMethod _ _) = True
    isMethod _ = False

irMethod :: Ident -> FnDef -> IRMonad [Definition]
irMethod cname fn@(FnDef p ret n args block) = do
  let newFuncName = nameToIdent $ astFromFIdent cname n
  let fnDef = TopClassFnDef NoPos $ FnDef p ret newFuncName ((classArg):args) block
  modify $ currentClass .~ Just (astFromIdent cname)
  irTopDefM fnDef
  where
    nameToIdent n = Ident $ nameToString n
    classArg = Arg NoPos (Class NoPos cname) (Ident "_this")



mallocType :: LLVM.AST.Type.Type -> LLVM.AST.Type.Type
mallocType typ = PointerType (FunctionType
  { resultType = PointerType (typ) (AddrSpace 0) -- Return type: i8*
  , argumentTypes = [IntegerType 32] -- Single argument: i32
  , isVarArg = False -- malloc is not variadic
  }) (AddrSpace 0)
genClassMalloc :: Ident -> IRMonad ()
genClassMalloc id = do
  let mallocName = getMallocName id -- Name of the malloc function
  fields <- gets $ Map.lookup (astFromIdent id) . (^. classFields) -- Retrieve class fields
  let fieldTypes = Prelude.map (\(_, t, _) -> t) $ fromJust fields -- Extract field types
  let classType = NamedTypeReference (astFromIdent id) -- The class type

  let mallocDef = GlobalDefinition $ functionDefaults
        { name = mallocName
        , parameters = ([], False) -- No parameters for the malloc function
        , returnType = PointerType classType (AddrSpace 0)
        , basicBlocks =
            [ BasicBlock
                (Name "entry")
                [
                      UnName 0 := GetElementPtr
        { inBounds = True
        , address = ConstantOperand $ C.Null (PointerType classType (AddrSpace 0)) -- Use null here
        , indices = [ConstantOperand $ C.Int 32 1]
        , LLVM.AST.metadata = []
        }
                , UnName 1 := PtrToInt
                    { operand0 = LocalReference (PointerType classType (AddrSpace 0)) (UnName 0)
                    , LLVM.AST.type' = IntegerType 32
                    , LLVM.AST.metadata = []
                    }
                , UnName 2 := Call
                    { tailCallKind = Nothing
                    , LLVM.AST.callingConvention = CC.C
                    , LLVM.AST.returnAttributes = []
                    , function = Right $ ConstantOperand $ C.GlobalReference (PointerType (mallocType(IntegerType 8) ) (AddrSpace 0)) (Name "_malloc")
                    , arguments = [(LocalReference (IntegerType 32) (UnName 1), [])]
                    , LLVM.AST.functionAttributes = []
                    , LLVM.AST.metadata = []
                    }
                , UnName 3 := BitCast
                    { operand0 = LocalReference (PointerType i8 (AddrSpace 0)) (UnName 2)
                    , LLVM.AST.type' = PointerType classType (AddrSpace 0)
                    , LLVM.AST.metadata = []
                    }
                ]
                (Do $ LLVM.AST.Ret (Just $ LocalReference (PointerType classType (AddrSpace 0)) (UnName 3)) [])
            ]
        }

  modify $ classDefs %~ (++ [mallocDef])

getMallocName :: Ident -> Name
getMallocName (Ident id) = mkName $ "_" ++ id ++ "_malloc"

genClassType :: Ident -> IRMonad ()
genClassType id = do
  fields <- gets $ Map.lookup (astFromIdent id) . (^. classFields)
  let fields_types = Prelude.map (\(_, t, _) -> astFromRType t) $ fromJust fields
  let vtable_type = PointerType (NamedTypeReference $ getClassVTypeName id) (AddrSpace 0)
  let class_type = StructureType { isPacked = False, elementTypes = vtable_type:fields_types }
  let classTypeDef = TypeDefinition (astFromIdent id) (Just class_type)
  modify $ classDefs %~ (++ [classTypeDef])

putFields :: Ident -> [ClassMember] -> IRMonad ()
putFields ident items = do
  let fields = filter isField items
  let fields' = map (\(ClassField _ t i) -> (astFromIdent i, fromType t)) fields
  let fields_with_index =   map (\((a, b), i) -> (a, b, i)) $ zip fields' [1..]
  modify $ classFields %~ Map.insert (astFromIdent ident) fields_with_index
  where isField (ClassField _ _ _) = True
        isField _ = False

putMethods :: Ident -> [ClassMember] -> IRMonad ()
putMethods ident items = do
  let methods = filter isMethod items
  let classParameter = RTClass ident
  let methods' = map (\(ClassMethod _ (FnDef _ ret i args _) ) -> (i, astFromFIdent ident i, fromType ret, classParameter : (getArgTypes args) )) methods
  let methods_with_index =   map (\((a,a', b, c), i) -> (a, a', b, c, i)) $ zip methods' [0..]
  modify $ classMethods %~ Map.insert (astFromIdent ident) methods_with_index
  where isMethod (ClassMethod _ _) = True
        isMethod _ = False
        getArgTypes args = map (\(Arg _ t _) -> fromType t) args

getClassType :: Ident -> IRMonad LLVM.AST.Type.Type
getClassType id = do
  pure $ PointerType (NamedTypeReference $ astFromIdent id) (AddrSpace 0)
astFromFIdent :: Ident -> Ident -> Name
astFromFIdent (Ident i) (Ident j) = mkName $ "_cf_" ++ i ++ "_" ++ j

genVType :: Ident -> IRMonad ()
genVType id = do
  methods <-  gets $ Map.lookup (astFromIdent id ). (^. classMethods)
  let methods_ftypes = Prelude.map getMethodType $ fromJust methods
  let vtype = StructureType { isPacked = False, elementTypes =  methods_ftypes }
  let vtypedef = TypeDefinition (getClassVTypeName id) (Just vtype)
  modify $ classDefs %~ (++ [vtypedef])

genVData :: Ident -> IRMonad ()
genVData id = do
  methods <-  gets $ Map.lookup (astFromIdent id ). (^. classMethods)
  let vdata = GlobalDefinition $ globalVariableDefaults {
    name = getClassVDataName id,
    initializer = Just $ C.Struct Nothing False $ map getMethodReference $ fromJust methods,
    LLVM.AST.Global.type' = NamedTypeReference $ getClassVTypeName id
  }
  modify $ classDefs %~ (++ [vdata])

getClassVDataName :: Ident -> Name
getClassVDataName (Ident i) = mkName $ "_"++ i ++ "_vdata"

getClassVTypeName :: Ident -> Name
getClassVTypeName (Ident i) = mkName $ "_"++ i ++ "_vtype"

getMethodReference :: ClassMethod -> C.Constant
getMethodReference (n,n', t, args, i) = C.GlobalReference (getMethodType (n,n', t, args, i)) n'

getMethodType :: ClassMethod ->  LLVM.AST.Type.Type
getMethodType (n, n', t, args, i) = do
  let args' = Prelude.map astFromRType args
  let res = PointerType (FunctionType {resultType = astFromRType t , argumentTypes =args'  , isVarArg = False}) (AddrSpace 0)
  res


updateExpr :: Expr -> Value -> IRMonad ()
updateExpr (EVar _  id) val = do
  updateIdent id val

updateExpr (EMember _ expr id) val = do
  clasRes <- irExprM expr
  env <- get
  let (classVar, classId) = case clasRes of
        VReference n (RTClass cId) -> (n, astFromIdent cId)
        _ -> error "this isn't a class!"
  let !fields = fromJust $ Map.lookup (classId)  $ env ^. classFields
  let (_, !typ, !i) = fromJust $ find (\(s, _, _) -> s == astFromIdent id) fields
  storeMemberVariable clasRes val i
  where
    getClassName (EVar _ id) = id
    getClassName _ = error "this isn't a class!"
    idToString (Ident s) = s

updateExpr (EArrGet _ expr1 expr2) val = do
  arrRes <- irExprM expr1
  indexRes <- irExprM expr2
  setArrayIndex arrRes indexRes val

updateExpr _ _ = error "Shouldn't happen"

setArrayIndex :: Value -> Value -> Value -> IRMonad ()
setArrayIndex arr@(VReference arrName (RTArr typ)) len val = do

  v3 <- getArrayIndexPointer arr len
  modify $ instrAcc %~ (++ [Do $ Store False v3 (vOp val) Nothing 0 []])
  
setArrayIndex _ _ _ = error "this isn't an array!"

genArr :: IRMonad ()
genArr = do
  let arrType = StructureType False [ PointerType (IntegerType 8) (AddrSpace 0), IntegerType 32]
  let arrTypeDef = TypeDefinition (astFromIdent arrIdent) (Just arrType)
  modify $ classFields %~ Map.insert (astFromIdent arrIdent) [("data", RTString, 1), ("length", RTInt, 0)]
  modify $ classMethods %~ Map.insert (astFromIdent arrIdent) []
  modify $ classDefs %~ (++ [arrTypeDef])
  genClassMalloc (Ident "_arr")

irProgramM :: Program -> IRMonad Module
irProgramM (Program _ topdefs) = do
  let cDefs = filter isClassDef topdefs
  let cExtDefs = filter isClassExtDef topdefs
  let fnDenfs = filter isFnDef topdefs
  genArr
  mapM_ collectSignature cDefs
  mapM_ collectSignature cExtDefs
  mapM_ collectSignature fnDenfs
  compiledCDefs <- mapM irTopDefM cDefs
  compiledCExtDefs <- mapM irTopDefM cExtDefs
  modify $ \env -> env & currentClass .~ Nothing
  compiledFnDefs <- mapM irTopDefM fnDenfs
  let compiledTopDefs' = concat $ compiledCDefs ++ compiledCExtDefs ++ compiledFnDefs
  env <- get
  let globalD = env ^. globalDefs
  let classD = env ^. classDefs
  let module' = defaultModule {
    moduleName = "program",
    moduleDefinitions = predifinedDecl ++ globalD ++ classD ++ compiledTopDefs'

  }
  pure module'
  where isClassDef (TopClassDef _ (ClassDef _ _ _)) = True
        isClassDef _ = False
        isClassExtDef (TopClassDef _ (ClassExtDef _ _ _ _)) = True
        isClassExtDef _ = False
        isFnDef (TopClassFnDef _ _) = True
        isFnDef _ = False



getIR :: Program -> IO (Either CompileException Module)
getIR program = runExceptT $ evalStateT (irProgramM program) emptyEnv

