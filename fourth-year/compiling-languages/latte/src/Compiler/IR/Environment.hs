{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.IR.Environment where

import           Grammar.Abs

import           Lens.Micro
import           Lens.Micro.TH
import qualified Data.Map     as Map

import           Data.Maybe
import           Prelude
import qualified Data.Text as T
import LLVM.AST hiding (Type)
import qualified LLVM.AST.Type as AST
import LLVM.AST (Operand(..))
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type (i32)
import          Common.RTypes
import          Compiler.IR.Utils
import LLVM.IRBuilder (add)
import LLVM.AST.CallingConvention (CallingConvention(C))
type Location = Integer

type ExpMap = Map.Map Expr Location

data Value = VInt Integer
            | VFalse
            | VTrue
            | VReference Name RawType
  deriving (Show, Eq)


data Env = Env
  { 
     _loc   :: Map.Map Ident Location 
  ,  _store :: Map.Map Location Value 
  ,  _currentBlockName :: Name
  , _emptyStringName :: Maybe Operand
  , _returnFlag :: Bool
  , _instrAcc :: [Named Instruction]
  , _cVarId :: Integer
  , _cBlockId :: Integer
  , _globalDefs :: [Definition]
  , _pBlocks :: [BasicBlock]
  , _nextLoc :: Location 
  } deriving (Show, Eq)

-- compile result (Ident, T.Text)
type CompileRes = [T.Text]

dummyLoc :: Location
dummyLoc = -1

dummy ::  CompileRes
dummy = []

makeLenses ''Env

identNamePredif :: [(Ident, Value)]
identNamePredif = map (\(i, v) -> (i, VReference (astFromIdent i) v)) predifinedFunctionsInternal


insertIdents :: [(Ident, Value)] -> Env -> Env
insertIdents idents env = foldl (\acc (i, v) -> insertIdent i v acc) env idents

emptyEnv :: Env
emptyEnv = insertIdents identNamePredif  Env { _loc = Map.empty, _nextLoc = 0, _store = Map.empty,
  _currentBlockName = mkName "entry", _instrAcc = [], _cVarId = 0, _cBlockId = 0, _globalDefs = [], _emptyStringName = Nothing, _returnFlag = False, _pBlocks = []}

updateIdent :: Ident -> Value -> Env -> Env 
updateIdent id val env = do
  let l = fromJust $  Map.lookup id (env ^. loc)
  env & store %~ Map.insert l val

getNexLoc :: Env -> (Location, Env) 
getNexLoc env = (env ^. nextLoc, env & nextLoc %~ (+1)) 
insertIdent :: Ident -> Value -> Env -> Env 
insertIdent id val env = do
  let (l, newEnv) = getNexLoc env
  newEnv & loc %~ Map.insert id l
    & store %~ Map.insert l val 
  
lookupIdent :: Ident -> Env -> Value 
lookupIdent id env = do
  let l = fromJust $  Map.lookup id (env ^. loc)
  fromJust $ Map.lookup l (env ^. store)

insertArgs :: [Arg] -> Env -> Env
insertArgs args env = foldl (flip insertArg) env args
insertArg :: Arg -> Env -> Env
insertArg (Arg _ t i) env = do
  let name = astFromIdent i
  insertIdent i (VReference name (fromType t)) env



insertInstruction :: Named Instruction -> Env -> Env
insertInstruction instr env = env & instrAcc %~ (++ [instr])




addCall :: Name -> Operand -> Operand -> Env -> Env
addCall i op1 op2 env = do
  env
    & instrAcc %~ (++ [i := Add False False op1 op2 [] ] )



vOp :: Value  -> Operand
vOp (VInt i)  = ConstantOperand (C.Int 32 i)
vOp VFalse  = ConstantOperand (C.Int 1 0)
vOp VTrue  = ConstantOperand (C.Int 1 1)
vOp (VReference id typ)  = LocalReference (astFromRType typ) id

toName :: Value -> Maybe Name
toName (VReference n _ ) = Just n
toName _ = Nothing

astIntOp :: Integer -> Operand
astIntOp i = ConstantOperand (C.Int 32  i)

astBoolOp :: Bool -> Operand
astBoolOp b = ConstantOperand (C.Int 1 (if b then 1 else 0))

retCall :: Value -> Named Terminator
retCall val  = Do $ LLVM.AST.Ret (Just $ vOp val) []

vretCall :: Named Terminator
vretCall = Do $ LLVM.AST.Ret Nothing []


stringCastCall :: Name -> Operand -> Env -> Env
stringCastCall i j env = do
  env
    & instrAcc %~ (++ [i := BitCast j (astFromRType RTString) []])

funcCall :: Name -> Ident -> [Operand] -> Env -> Env
funcCall i ident args env = do
  let fValue = lookupIdent ident env

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
  env
    & instrAcc %~ (++ [namedCall])
    -- return of function is stored in i
  where
    getVType (VReference _ t) = t
    getVType _ = error "Not a function"
    fromIdent (Ident s) = s

truncCall :: Name -> RawType -> Operand -> Env -> Env
truncCall i typ op env = do
  env
    & instrAcc %~ (++ [i := Trunc op (astFromRType typ) []])

phiCall :: Name -> RawType -> [(Operand, Name)] -> Env -> Env
phiCall i typ ops env = env & instrAcc %~ (++ [i := Phi (astFromRType typ) ops []]) 

getReturnType :: RawType -> RawType
getReturnType (RTFun returnType _) = returnType
getReturnType _ = error "Not a function type"

binOpCall :: Name -> RawType -> (Operand -> Operand -> InstructionMetadata -> Instruction) -> Operand -> Operand -> Env -> Env
binOpCall name typ i op1 op2 env = do
  env
  & instrAcc %~ (++ [name := i op1 op2 []])

resetBlock :: Env -> Env
resetBlock env = env & instrAcc .~ [] 
  & currentBlockName .~  astFromIdent (Ident "wrong")


