module Compiler.IR.Monad where

import           Control.Monad.Except
import           Control.Monad.State
import           Compiler.IR.Environment
import           Common.Exception

import           Lens.Micro
import          Grammar.Abs
import          Common.RTypes
import           LLVM.AST hiding (Type)
import qualified Data.Map as Map
import Data.Maybe
import Data.List (find)
import Compiler.IR.Utils
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as C
import LLVM.AST.CallingConvention (CallingConvention(C))
import Lens.Micro.Extras
import LLVM.AST.AddrSpace
import Compiler.Optimizer.Optimizer (nameToString)
import qualified LLVM.AST.Type

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
  pure $ mkName $ "_B" ++ show (env ^. cBlockId)

getNextVarName :: IRMonad Name
getNextVarName = do
  env <- get
  let name =  mkName $ "_v" ++ show (env ^. cVarId)
  modify $ cVarId %~ (+1)
  pure name 

finalizeBlock :: Named Terminator -> IRMonad ()
finalizeBlock terminator  = do
  env <- get
  let blockname = env ^.  currentBlockName
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

fullStoreVar :: Ident -> RawType -> IRMonad Name
fullStoreVar ident typ  = do


    name <- case typ of
        RTFun _ _ -> pure $ mkName $ fromIdent ident
        _ -> getNextVarName
    modify $ insertIdent ident (VReference name typ)
    pure name
    where
        fromIdent :: Ident -> String
        fromIdent (Ident s) = s


lookupIdent :: Ident  -> IRMonad Value 
lookupIdent (Ident "self") = lookupIdent (Ident "_this")
lookupIdent id = do
    env <- get
    let classs = env ^. currentClass
    case classs of 
      Nothing -> _lookupVar id
      Just na -> do 
        let fields = fromJust $ Map.lookup na (env ^. classFields)
        let field = find (\(n, _, _) -> n == astFromIdent id) fields
        case field of 
          Nothing -> _lookupVar id
          Just (_, typ, i) -> do 

            var' <- getNextVarName
            loadReference var' na i
            var <- getNextVarName
            modify $ instrAcc %~ (++ [var := Load False (LocalReference (PointerType (astFromRType typ) (AddrSpace 0)) (var')) Nothing 0 []])
            pure $ VReference  (var) typ
        where
          getN (LocalReference _ n) = n
          getN _ = mkName ""

_lookupVar:: Ident -> IRMonad(Value)
_lookupVar id = do
  env <- get
  let l = fromJust $  Map.lookup id (env ^. loc)
  let var  = fromJust $ Map.lookup l (env ^. store)
  case var of 
    VReference na (RTClass id) -> do 
      varName <- getNextVarName
      modify $ instrAcc %~ (++ [varName := Load False (LocalReference (myPointerType $myPointerType $  NamedTypeReference $ astFromIdent id) na) Nothing 0 []])
      pure $ VReference varName (RTClass id)
      
    _ -> pure var 

astClassT :: Ident -> LLVM.AST.Type.Type
astClassT (Ident id) = NamedTypeReference $ mkName id

astClassTN :: Name-> LLVM.AST.Type.Type
astClassTN n = NamedTypeReference $ n
astRef :: LLVM.AST.Type.Type -> LLVM.AST.Type.Type
astRef t = PointerType t (AddrSpace 0)

loadReference :: Name -> Name -> Int -> IRMonad()
loadReference var cl offset = do


  let classType = astClassTN cl
  let classRefType = astRef classType
  classValue <- lookupIdent (Ident "_this")
  case classValue of 
    VReference na (RTClass _) -> do 
      
      modify $ instrAcc %~ (++ [var := GetElementPtr False (LocalReference classRefType na ) [iOp 0, iOp (fromIntegral offset)] []])
      pure ()
    _ -> do 
      error "Not a class"


loadMemberReference :: Value  -> Int -> IRMonad(Name)
loadMemberReference (VReference n (RTArr _) ) offset = loadMemberReference (VReference n (RTClass arrIdent)) offset
loadMemberReference classValue  offset = do

  case classValue of 
    VReference na (RTClass classId) -> do 
      let classType = astClassTN $ astFromIdent classId
      let classRefType = astRef classType 
      var <- getNextVarName
      modify $ instrAcc %~ (++ [var := GetElementPtr False (LocalReference classRefType na ) [iOp 0, iOp (fromIntegral offset)] []])
      pure var
    _ -> do 
      error "Not a class"

myPointerType :: LLVM.AST.Type.Type -> LLVM.AST.Type.Type
myPointerType t = PointerType t (AddrSpace 0) 

funcCall :: Name -> Ident -> [Operand] -> IRMonad ()
funcCall i ident args  = do
  fValue <- lookupIdent ident 

  let ftype = getVType fValue

  let call = Call {
    tailCallKind = Nothing,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ ConstantOperand $ C.GlobalReference (astFromRType ftype) (mkName $ fromIdent ident),
    arguments = zip args (repeat []),
    functionAttributes = [],
    metadata = []
  }
  let returnType = getReturnType $ ftype
  let namedCall = if returnType == RTVoid then Do call else i := call
  modify $ instrAcc %~ (++ [namedCall])
  where
    getVType (VReference _ t) = t
    getVType _ = error "Not a function"
    fromIdent (Ident s) = s

funcCallWType :: Name -> Ident -> RawType -> [Operand] -> IRMonad ()
funcCallWType i ident ftype args  = do

  let call = Call {
    tailCallKind = Nothing,
    callingConvention = C,
    returnAttributes = [],
    function = Right $ LocalReference (astFromRType ftype) (mkName $ fromIdent ident),
    arguments = zip args (repeat []),
    functionAttributes = [],
    metadata = []
  }
  let returnType = getReturnType $ ftype
  let namedCall = if returnType == RTVoid then Do call else i := call
  modify $ instrAcc %~ (++ [namedCall])
  where
    getVType (VReference _ t) = t
    getVType _ = error "Not a function"
    fromIdent (Ident s) = s

updateIdent :: Ident -> Value -> IRMonad ()
updateIdent id val = do
  let varName = astFromIdent id
  env <- get
  let classs = env ^. currentClass
  case classs of
    Just c -> do
      let fields = fromJust $ Map.lookup c (env ^. classFields)
      let field = find (\(n, _, _) -> n == varName) fields
      case field of
        Just (_, typ, i) -> do
          storeClassVariable c typ i val 
        Nothing -> do
          storeRegularVar id val
    Nothing -> do
      storeRegularVar id val

storeRegularVar :: Ident -> Value -> IRMonad ()
storeRegularVar id val = do 
      env <- get 
      let l = fromJust $  Map.lookup id (env ^. loc)
      let currentVal = fromJust $ Map.lookup l (env ^. store)
      case (currentVal, val) of
        (VReference na (RTClass id), VReference na' (RTClass id')) -> do 
        -- store value in 
          (instr, val') <- getBitCastInstr (val, RTClass id) 
          modify $ instrAcc %~ (++ instr ++[Do $ Store False (LocalReference (astRef $ astRef (astClassT id)) na) (vOp val') Nothing 0 []])
        
        _ -> modify $ store %~ Map.insert l val

  


getBitCastInstr :: (Value, RawType) -> IRMonad ([Named Instruction], Value)
getBitCastInstr ((VReference n (RTClass id')), RTClass id) = do
  if id == id' then pure ([], VReference n (RTClass id))
  else do 
    v <- getNextVarName
    let instr =  [v := BitCast (LocalReference (astFromRType (RTClass id')) n) (astFromRType (RTClass id)) []]
    pure (instr, VReference v (RTClass id))
getBitCastInstr (v, typ) = pure ([], v)
      

_lookupBlockVar:: Ident -> Name -> IRMonad(Value)
_lookupBlockVar id na = do
    newVar <- getNextVarName
    let ref = myPointerType $ myPointerType (NamedTypeReference $ astFromIdent id)
    modify $ instrAcc %~ (++ [newVar := Load False (LocalReference ref na) Nothing 0 []])
    pure $ VReference newVar (RTClass id)

storeClassVariable :: Name -> RawType -> Int -> Value -> IRMonad ()
storeClassVariable className typ offset val = do 
  var' <- getNextVarName
  loadReference var' className offset
  
  
  modify $  instrAcc %~ (++ [Do $ Store False (LocalReference (myPointerType $ astFromRType typ ) var') (vOp val)Nothing 0 []])
  pure ()


storeMemberVariable ::  Value -> Value -> Int -> IRMonad ()
storeMemberVariable classVal@(VReference _ (RTClass id)) val offset = do
  var' <- loadMemberReference classVal offset 

  modify $  instrAcc %~ (++ [Do $ Store False (LocalReference (myPointerType $ astFromValue val ) var') (vOp val )Nothing 0 []])
  pure ()

storeMemberVariable _ _ _ = error "Not a class"

astFromValue :: Value -> LLVM.AST.Type.Type
astFromValue (VReference _ t) = astFromRType t
astFromValue VInt {}= LLVM.AST.Type.i32
astFromValue VTrue = LLVM.AST.Type.i1
astFromValue VFalse = LLVM.AST.Type.i1


changeVType :: Value -> RawType -> IRMonad Value 
changeVType (VReference n (RTClass id)) (RTClass id') = do 
  if id == id' then pure $ VReference n (RTClass id)
  else do 
    newVar <- getNextVarName
    modify $ instrAcc %~ (++ [newVar := BitCast (LocalReference (astFromRType (RTClass id)) n) (astFromRType (RTClass id')) []])
    pure $ VReference newVar (RTClass id')
changeVType v _ = pure v

retCall :: Value -> IRMonad(Named Terminator)
retCall val  = do 
  expected <- gets $ (^. currentReturnType) 
  newVal <- changeVType val expected
  pure $ Do $ LLVM.AST.Ret (Just $ vOp $ newVal) []