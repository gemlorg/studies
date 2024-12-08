module Compiler.IR.Monad where
    
import           Control.Monad.Except
import           Control.Monad.State
import           Compiler.IR.Environment
import           Common.Exception

import           Lens.Micro
import          Grammar.Abs
import qualified Data.Map as Map
import          Common.RTypes
import           LLVM.AST hiding (Type)

type IRMonad a = StateT Env (ExceptT CompileException IO) a


putBlockName :: Name -> Env -> Env 
putBlockName name env = env & currentBlockName .~ name 

condJump :: Operand -> Name -> Name ->Named Terminator
condJump cond b1 b2 = do 
  Do $ CondBr cond b1 b2 []

jump :: Name -> Named Terminator
jump name = Do $ Br name []

getNextBlockName :: IRMonad Name 
getNextBlockName = do
  modify $ cBlockId %~ (+1)
  env <- get
  pure $ mkName $ "_B" ++ (show $ env ^. cBlockId)

getNextVarName :: IRMonad Name
getNextVarName = do 
  modify $ cVarId %~ (+1)
  env <- get
  pure $ mkName $ "_v" ++ (show $ env ^. cVarId)

finalizeBlock :: Named Terminator -> IRMonad ()
finalizeBlock terminator  = do 
  env <- get 
  let blockname = env ^.  currentBlockName
  -- liftIO $ print $ "finalizing block of name " ++ show blockname ++ " and retflag " ++ (show $ env ^. returnFlag)
  if blockname == mkName "wrong" || env ^. returnFlag then do
    pure ()
  else do
    let instr = env ^. instrAcc  
    modify resetBlock  
    modify $ addBlocks [BasicBlock blockname instr terminator]


finalizeRetBlock :: Named Terminator -> IRMonad ()
finalizeRetBlock terminator  = do 
  env <- get 
  let blockname = env ^.  currentBlockName 
  if blockname == mkName "wrong" then do
    pure ()
  else do
    let instr = env ^. instrAcc  
    modify resetBlock  
    modify $ addBlocks [BasicBlock blockname instr terminator]

addBlocks :: [BasicBlock] -> Env -> Env 
addBlocks blocks env = env & pBlocks %~ (++blocks)

fullStoreVar :: Ident -> RawType -> IRMonad(Name)
fullStoreVar ident typ  = do


    name <- case typ of  
        RTFun _ _ -> pure $ mkName $ fromIdent ident
        _ -> getNextVarName
    modify $ insertIdent ident (VReference name typ)
    pure name
    where 
        fromIdent :: Ident -> String
        fromIdent (Ident s) = s
