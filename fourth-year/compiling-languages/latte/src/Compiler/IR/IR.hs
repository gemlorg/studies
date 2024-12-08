{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.IR.IR where


import           Grammar.Abs
import           Grammar.Par         (myLexer, pProgram)

import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (hPrint, stderr)
import           Control.Monad.Except
import           Control.Monad.State
import   qualified        Data.Text   as T
import   qualified        Data.Text.IO as TIO
import Data.Text.Lazy (unpack)
import Text.Printf (printf)
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Lens.Micro
import Common.Exception

import LLVM.AST.Type as AST hiding (Type)
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.AST.Global
-- import LLVM.Context
-- import LLVM.Module
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.AST hiding (Type)
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Operand as O
import Compiler.IR.Pretty (ppllvm) -- Import pretty-printer

import Common.GrammarUtils
import Compiler.IR.Monad
import Compiler.IR.Environment
import Common.RTypes
import Compiler.IR.Utils ( astFromArgs, astFromType, astFromIdent, predifinedDecl)
import LLVM.AST.Linkage
import LLVM.AST.AddrSpace
import Data.Maybe

-- simple :: Module
-- simple = buildModule "exampleModule" $ mdo
--   function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
--     _entry <- block `named` "entry"
--     cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
--     condBr cond ifThen ifElse
--     ifThen <- block
--     trVal <- add a (ConstantOperand (C.Int 32 0))
--     br ifExit
--     ifElse <- block `named` "if.else"
--     flVal <- add a (ConstantOperand (C.Int 32 0))
--     br ifExit
--     ifExit <- block `named` "if.exit"
--     r <- phi [(trVal, ifThen), (flVal, ifElse)]
--     ret r

--   function "plus" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
--     _entry <- block `named` "entry2"
--     r <- add x y
--     ret r
irExprM :: Expr -> IRMonad Value
irExprM (EVar _ ident) = gets $ lookupIdent ident
irExprM (ELitInt _  i) = do
  pure $ VInt i

irExprM (EString _ s) = do
  str <- addGlobalString s
  var <- getNextVarName
  modify $ stringCastCall var str
  pure $ VReference var RTString

irExprM (ELitTrue _) = do
  pure VTrue

irExprM (ELitFalse _) = do
  pure VFalse

irExprM (EApp _ ident exprs) = do
  argNames <- mapM irExprM exprs
  let args = map vOp argNames
  name <- gets $ lookupIdent ident
  var <- getNextVarName
  modify $ funcCall var ident args
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
      modify $ binOpCall var RTBool (Xor)  (astBoolOp True) (vOp exprRes )
      pure $ VReference var RTBool

irExprM (EAdd _ expr1 op expr2) = do
  expr1Res <- irExprM expr1
  expr2Res <- irExprM expr2
  case (expr1Res, expr2Res) of
    (VInt i1, VInt i2) -> do
      pure $ VInt $ case op of
        Plus _ -> i1 + i2
        Minus _ -> i1 - i2
    (VReference _ RTString, VReference _ RTString) -> do
      var <- getNextVarName
      modify $ funcCall var (Ident "_concatStrings") [vOp expr1Res, vOp expr2Res]
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
    (VInt i1, VInt i2) -> do
      pure $ VInt $ case op of
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
  env <- get
  let op' = case op of
        LTH _ -> I.ICmp P.SLT
        LE _ -> I.ICmp P.SLE
        GTH _ -> I.ICmp P.SGT
        GE _ -> do
            -- if fromJust (env ^. Compiler.IR.Environment.store . at expr1Res) == RTString
            --   then do
            --     -- fname <- lookupIdent (Ident "_concatStrings")
            --     -- modify $ funcCall var (mkName "_concatStrings") [astIdOp expr1Res env, astIdOp expr2Res env]
            --     pure var
              -- else do
              I.ICmp P.SGE
        EQU _ -> I.ICmp P.EQ
        NE _ -> I.ICmp P.NE
  case (expr1Res, expr2Res) of
      (VReference _ RTString, VReference _ RTString) -> do
        case op of
          EQU _ -> do
            modify $ funcCall var (Ident "_compareStrings") [vOp expr1Res , vOp expr2Res ]
            newVar <- getNextVarName
            modify $ truncCall newVar RTBool (LocalReference (IntegerType 32) var)
            pure $ VReference newVar RTBool
          NE _ -> do
            modify $ funcCall var (Ident  "_compareStrings") [vOp expr1Res , vOp expr2Res ]
            newVar <- getNextVarName
            modify $ truncCall newVar RTBool (LocalReference (IntegerType 32) var)
            newVar2 <- getNextVarName
            modify $ binOpCall newVar2 RTBool Xor (LocalReference (IntegerType 1) newVar) (astBoolOp True)
            pure $ VReference newVar2 RTBool
          _ -> do
            error "this should not happen"
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
  -- case op of
  --   EQU _ -> do
  --     if  (env ^. Compiler.IR.Environment.store . at ( toName  expr1Res)) == Just RTString then do
  --       modify $ funcCall var (mkName "_compareStrings") [vOp expr1Res env, vOp expr2Res env]
  --       newVar <- getNextVarName
  --       modify $ truncCall newVar RTBool (LocalReference (IntegerType 32) var)
  --       pure $ VReference newVar
  --     else do
  --       modify $ binOpCall var RTBool op' (vOp expr1Res env) (vOp expr2Res env)
  --       pure $ VReference var 
  --   NE _ -> do
  --     if (env ^. Compiler.IR.Environment.store . at ( toName expr1Res)) == Just RTString then do
  --       modify $ funcCall var (mkName "_compareStrings") [vOp expr1Res env, vOp expr2Res env]
  --       newVar <- getNextVarName
  --       modify $ truncCall newVar RTBool (LocalReference (IntegerType 32) var)
  --       newVar2 <- getNextVarName 

  --       modify $ binOpCall newVar2 RTBool Xor (LocalReference (IntegerType 1) newVar) (astBoolOp True)
  --       pure $ VReference newVar2
  --     else do
  --       modify $ binOpCall var RTBool op' (vOp expr1Res env) (vOp expr2Res env)
  --       pure $ VReference var
  --   _ -> do
  --     modify $ binOpCall var RTBool op' (vOp expr1Res env) (vOp expr2Res env)
  --     pure $ VReference var
irExprM (EAnd _ expr1 expr2) = do
  expr1Res <- irExprM expr1
  -- liftIO $ print $ "calculating expr of res " ++ show expr1Res
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


  -- expr2Res <- irExprM expr2
  -- case (expr1Res, expr2Res) of 
  --   (VTrue, VTrue) -> pure VTrue
  --   (VFalse, _ ) -> pure VFalse
  --   (_, VFalse) -> pure VFalse
  --   _ -> do
  --     var <- getNextVarName
  --     modify $ binOpCall var RTBool And (vOp expr1Res) (vOp expr2Res )
  --     pure $ VReference var RTBool 
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

addGlobalString :: String -> IRMonad Operand
addGlobalString s = do
  var <- getNextVarName
  let def =  GlobalDefinition globalVariableDefaults {
    name = var,
    linkage = Private,
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
    _ -> error "this type isn't supposed to have default value"
  pure ()


declareVar :: Type -> Item -> IRMonad [BasicBlock]
declareVar typ (NoInit _ ident) = do
  _ <- initVar ident (fromType typ)
  pure []

declareVar typ (Init _ ident expr) = do
  exprRes <- irExprM expr
  modify $ insertIdent ident exprRes
  pure []
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
  irM (Decl _ typ items) = do
    mapM_ (declareVar typ) items
  irM (Ass _ ident expr) = do
    expRes <- irExprM expr
    modify $ updateIdent ident expRes
    pure ()
  irM (Incr _ ident) = do
    res <- irExprM (EAdd  NoPos (EVar NoPos ident) (Plus NoPos) (ELitInt NoPos 1))
    modify $ updateIdent ident res
    pure ()
  irM (Decr _ ident) = do
    res <- irExprM (EAdd  NoPos (EVar NoPos ident) (Minus NoPos) (ELitInt NoPos 1))
    modify $ updateIdent ident res
    pure ()
  irM (Grammar.Abs.VRet _ ) = do
    finalizeRetBlock vretCall
    modify $ \env -> env & returnFlag .~ True
  irM (Grammar.Abs.Ret _  expr) = do
    expRes <- irExprM expr
    finalizeRetBlock (retCall expRes)
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
        -- put phi instructions to merge the two branches  
        let idents = Map.keys $ env ^. loc
        zipVars idents env env'

        pure $ ()

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
        -- let ret1 = get >>= returnFlag
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
    registerVars $ Map.keys $ env ^. loc
    env' <- get
    irM stmt
    env'' <- get
    let modifiedVars = compareVars (Map.keys $ env ^. loc) env' env''
    put envSave
    registeredVars <- registerVars modifiedVars
    irM stmt
    env''' <- get
    finalizeBlock (jump condBlockName)
    modify $ putBlockName condBlockName
    modify $ \denv -> denv & returnFlag .~ (env ^. returnFlag)
    zipVarsWhile registeredVars env env'''
    expRes <- irExprM expr
    case expRes of 
      VTrue -> finalizeBlock (jump loopBlockName) 
      VFalse -> finalizeBlock (jump afterBlockName)
      _ -> finalizeBlock (condJump (vOp expRes) loopBlockName afterBlockName)
    modify $ putBlockName afterBlockName


  irM (SExp _ expr) = do
    _ <- irExprM expr
    pure ()

registerVars :: [Ident] -> IRMonad ([(Ident, Name)])
registerVars [] = pure []
registerVars (i:is) = do
  newVarName <- getNextVarName
  env <- get
  let v = lookupIdent i env
  let typ = toTypeV v
  modify $ insertIdent i (VReference newVarName typ)
  rest <- registerVars is
  pure $ (i, newVarName) : rest

compareVars :: [Ident] -> Env -> Env -> [Ident]
compareVars [] _ _ = []
compareVars (i:is) env1 env2 = do
  let v1 = lookupIdent i env1
  let v2 = lookupIdent i env2
  if v1 == v2 then compareVars is env1 env2 else i:compareVars is env1 env2

zipVarsWhile :: [(Ident, Name)] -> Env -> Env -> IRMonad ()
zipVarsWhile [] _ _ = pure ()
zipVarsWhile ((i, n):is) env1 env2 = do
  let v1 = lookupIdent i env1
  let v2 = lookupIdent i env2
  let b1 = env1 ^. currentBlockName 
  let b2 = env2 ^. currentBlockName
  let typ = toTypeV v1
  unless (v1 == v2) $ do
    modify $ phiCall n typ [(vOp v1, b1), (vOp v2, b2)]
    modify $ updateIdent i (VReference n typ)
  zipVarsWhile is env1 env2

zipVars :: [Ident] -> Env -> Env ->  IRMonad()
zipVars [] _ _ = pure ()
zipVars (i:is) env1 env2= do
  let block1 = env1 ^. currentBlockName
  let block2 = env2 ^. currentBlockName
  let v1 = lookupIdent i env1
  let v2 = lookupIdent i env2
  let typ = toTypeV v1
  unless (v1 == v2) $ do
    newName <- getNextVarName
    modify $ phiCall newName typ [(vOp v1, block1), (vOp v2, block2)]
    modify $ updateIdent i ( VReference newName typ)
  zipVars is env1 env2
toTypeV :: Value -> RawType
toTypeV (VInt _) = RTInt
toTypeV VTrue = RTBool
toTypeV VFalse = RTBool
toTypeV (VReference _ t) = t
collectSignature :: TopDef -> IRMonad ()
collectSignature (FnDef _ ret ident args _ ) = do
  let rawArgTypes = fromArgs args
  let rawRet = fromType ret
  let funType = RTFun rawRet rawArgTypes
  _ <- fullStoreVar ident funType
  pure ()

irTopDefM :: TopDef -> IRMonad Definition
irTopDefM (FnDef _ ret ident args block ) = do
  -- let rawArgTypes = fromArgs args
  -- let rawRet = fromType ret 
  -- let funType = RTFun rawRet rawArgTypes
  -- env <- get 
  -- put $ insertFun ident funType env
  modify $ putBlockName (mkName "entry")
  modify $ \env -> env & returnFlag .~ False
  modify $ \env -> env & pBlocks .~ []
  modify $ insertArgs args
  irM block
  env <- get
  if (fromType ret == RTVoid) && env ^. currentBlockName /= mkName "wrong"
                then  finalizeRetBlock vretCall else pure ()
  env' <- get
  let blocks = env' ^. pBlocks
  -- add block with just return statement if the function is void 
  let func' = GlobalDefinition $ functionDefaults {
    name = astFromIdent ident,
    parameters = astFromArgs args,
    returnType = astFromType ret,
    basicBlocks = blocks
  }
  -- let func' = function (astFromIdent ident) (astFromArgs args) (astFromType ret) $ \_ -> compiledBlock
  pure func'






irProgramM :: Program -> IRMonad Module
irProgramM (Program _ topdefs) = do
  mapM_ collectSignature topdefs
  env <- get
  -- liftIO $ print env
  compiledTopDefs <- mapM irTopDefM topdefs
  env <- get
  let globalD = env ^. globalDefs
  -- liftIO $ print $ show  globalDefs
  let module' = defaultModule {
    moduleName = "program",
    moduleDefinitions = predifinedDecl ++ globalD ++ compiledTopDefs

  }
  pure module'



getIR :: Program -> IO (Either CompileException Module)
getIR program = do
  res <- runExceptT $ evalStateT (irProgramM program) emptyEnv
  -- putStrLn $ unpack $ ppllvm 
  -- case res of
  --   Right r -> putStrLn $ unpack $ ppllvm r
  --   _ -> pure ()
  pure res
