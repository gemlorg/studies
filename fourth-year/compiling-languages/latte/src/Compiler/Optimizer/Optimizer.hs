{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Compiler.Optimizer.Optimizer where
import Control.Monad.Except (runExceptT, MonadIO (liftIO), when, foldM, MonadTrans (lift))
import LLVM.AST
import Compiler.Optimizer.Monad
import LLVM.AST.Global
import Data.Map
import Control.Monad.RWS.Class (MonadState(get), MonadWriter (pass))
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List
import LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import Compiler.IR.Utils (ioDecls, astPredifinedFunctions)
import Data.ByteString.Short (fromShort, toShort)
import Data.ByteString.Char8 (unpack, empty)
import Control.Monad.State hiding (void)
import Lens.Micro.TH
import Lens.Micro
import Lens.Micro.Extras
import LLVM.AST.Typed
import Compiler.IR.Pretty (ppll)
import qualified Data.Text.Lazy
-- and on maps requires phi!!! 
-- function inlining: tests + description 
-- do quiz llvm, jvm 
-- 5.3 + 4.25 + 5.9 + 27.9 = 43.15
class GCSE a where
    gcse :: a -> IO a

instance GCSE Module where
    gcse llvm = do
        gcse_topdefs <- mapM gcse (moduleDefinitions llvm)
        return llvm { moduleDefinitions = gcse_topdefs }

instance GCSE Definition where
    gcse (GlobalDefinition f) = GlobalDefinition <$> gcse f
    gcse d = return d

instance GCSE Global where
    gcse f@(Function {..}) = do
        gcse_blocks <- _gcse basicBlocks
        return f { basicBlocks = gcse_blocks }
    gcse f = return f

instance Ord Instruction where
    compare :: Instruction -> Instruction -> Ordering
    compare a b = compare (show a) (show b)
type ExprMap = Map Instruction Name
newExprMap :: ExprMap
newExprMap = Map.empty
type VariableReplacementMap = Map Name Name
newVariableReplacementMap :: VariableReplacementMap
newVariableReplacementMap = Map.empty
type BlockOptimizedMap = Map Name Bool


type BlockEnv = (ExprMap, VariableReplacementMap)
type BlockEnvMap = Map Name BlockEnv
type GraphMap = Map Name [Name]
type GEnv = (GraphMap, BlockEnvMap)

emptyBlockEnv :: BlockEnv
emptyBlockEnv = (newExprMap, newVariableReplacementMap)

_gcse :: [BasicBlock] -> IO [BasicBlock]
_gcse blocks = do
    -- the entry block should be in the block_map
    -- if not return 
    if blocks == [] then return blocks else do
        _optimizeBlocks blocks
    where
        _optimizeBlocks :: [BasicBlock] -> IO ([BasicBlock])
        _optimizeBlocks blocks = do
            blocks' <- passGCSE blocks
            if blocks == blocks' then return blocks' else _optimizeBlocks blocks'

passGCSE :: [BasicBlock] -> IO([BasicBlock])
passGCSE blocks =  do
    cf_graph <- buildCFG blocks
    let blockEnvMap = Map.empty
    let env = (cf_graph, blockEnvMap)
    (blocks', _) <- _passGCSE blocks env $ mkName "entry"
    return blocks'


_passGCSE :: [BasicBlock] -> GEnv -> Name -> IO([BasicBlock], GEnv)
_passGCSE blocks envMap name = do

    -- can be optimised 
    let i = fromJust $ Data.List.findIndex (\(BasicBlock n _ _) -> n == name) blocks
    let myBlock = blocks !! i
    -- let (exp_map, var_map, block_map, cf_graph) = env
    let (cf_graph, block_map) = envMap
    let predecessors = fromJust $ Map.lookup name cf_graph
    let successors = getSuccessorsBlock myBlock
    -- if not all predecessors have been visited yet, skip
    if  isJust (Map.lookup name block_map)  ||  not (all (\p -> isJust (Map.lookup p block_map) || elem p successors
        ) predecessors) then do
        return (blocks, envMap)
    else do
        -- if name == mkName "entry" then let env = emptyBlockEnv 
        -- else do 
        --     let maps = Prelude.map (\p -> fromJust $ Map.lookup p block_map) predecessors
        --     let env = Prelude.foldl1 Map.intersection $ Prelude.map (\(a, _, _) -> a) maps
        -- replace ma
        let maps = catMaybes $ Prelude.map (\p -> Map.lookup p block_map) predecessors
        -- remove Nothings 
        let env = if name == mkName "entry" then emptyBlockEnv else _combineMaps maps
        let (exp_map, var_map) = env
        (myBlock', exp_map', var_map') <- _gcseBlockBody myBlock exp_map var_map
        -- insert optimised block 
        let blocks' = Prelude.take i blocks ++ [myBlock'] ++ Prelude.drop (i + 1) blocks
        let envMap' = Map.insert name (exp_map', var_map') block_map
        let successors = getSuccessorsBlock myBlock'
        (blocks'', env'') <- foldM (\(bs, e) s ->  _passGCSE bs e s) (blocks', (cf_graph, envMap')) successors
        let myBlock'' = blocks'' !! i
        let (_, blockEnvMap'') = env''
        let (_, varMap'') = fromJust $ Map.lookup name blockEnvMap''
        let myBlock''' = replaceBlockVars myBlock'' varMap''
        return (Prelude.take i blocks'' ++ [myBlock'''] ++ Prelude.drop (i + 1) blocks'', env'')

replaceBlockVars :: BasicBlock -> VariableReplacementMap -> BasicBlock
replaceBlockVars (BasicBlock name instrs term) var_map = do
    let instrs' = Prelude.map (replaceVarInstr var_map) instrs
    let term' = replaceTermVars term var_map
    BasicBlock name instrs' term'

replaceVarInstr :: VariableReplacementMap -> Named Instruction -> Named Instruction
replaceVarInstr var_map (na := instr) = na := _replaceVarInstr instr var_map
replaceVarInstr var_map (Do instr) = Do $ _replaceVarInstr instr var_map

_combineMaps :: [BlockEnv] -> BlockEnv
_combineMaps maps = do
    -- combine maps, if two different expressions but different name, add phi call 
    let (exp_maps, var_maps) = Prelude.unzip maps
    let all_expr_keys = Prelude.foldl (\acc m -> acc ++ Map.keys m) [] exp_maps
    -- filter out the expressions that are not in all maps or have different values 
    let common_expr_keys = Prelude.filter (\k -> all (\m -> Map.lookup k m == Map.lookup k (head exp_maps)) exp_maps) all_expr_keys
    -- new map is the common expressions with values from the first map
    let new_exp_map = Map.fromList $ Prelude.map (\k -> (k, fromJust $ Map.lookup k (head exp_maps))) common_expr_keys
    -- expr map is values that are in all values 
    -- here
    let new_var_map = Prelude.foldl1 Map.union var_maps
    (new_exp_map, new_var_map)


_gcseBlockBody :: BasicBlock -> ExprMap -> VariableReplacementMap -> IO(BasicBlock, ExprMap, VariableReplacementMap)
_gcseBlockBody block exp_map var_map = do
    let (BasicBlock name instrs term) = block
    (instrs', exp_map', var_map') <- foldM (\(i, e, v) instr -> _gcseInstr instr i e v) ([], exp_map, var_map) instrs
    let term' = replaceTermVars term var_map'
    return (BasicBlock name (reverse instrs') term', exp_map', var_map')
    where
        _gcseInstr :: Named Instruction -> [Named Instruction] -> ExprMap -> VariableReplacementMap -> IO([Named Instruction], ExprMap, VariableReplacementMap)
        _gcseInstr instr instrs exp_map var_map = do

            case instr of
                na := in' -> do
                    let in'' = _replaceVarInstr in' var_map
                    case Map.lookup in'' exp_map of
                        Just na' -> do
                            if isCall in' then return (na := in'' :instrs, exp_map, var_map) else
                                return (instrs, exp_map, Map.insert na na' var_map)
                        Nothing -> return ((na := in'' ) : instrs, Map.insert in'' na exp_map, var_map)
                Do in' -> do
                    let in'' = _replaceVarInstr in' var_map
                    return (Do in'' : instrs, exp_map, var_map)
        isCall :: Instruction -> Bool
        -- get the name of the function and check in the list 
        isCall Call {} = True
        isCall Store {} = True
        isCall Load {} = True
        isCall Alloca {} = True
        isCall BitCast {} = True
        isCall GetElementPtr {} = True
        isCall _ = False


_replaceVar :: Operand -> VariableReplacementMap -> Operand
_replaceVar op vm = case op of
    LocalReference t n -> case Map.lookup n vm of
        Just n' -> LocalReference t n'
        Nothing -> op
    _ -> op

replaceTermVars :: Named Terminator -> VariableReplacementMap -> (Named Terminator)
replaceTermVars term var_map = case term of
    Do (Ret (Just op) _) -> do
        let op' = _replaceVar op var_map
        Do (Ret (Just op') [])
    Do (CondBr op t f _) -> do
        let op' = _replaceVar op var_map
        Do (CondBr op' t f [])
    _ -> term


_replaceVarInstr :: Instruction -> VariableReplacementMap -> Instruction
_replaceVarInstr instr vm = case instr of
    Add b b' op op' x0 -> Add b b' (_replaceVar op vm) (_replaceVar op' vm) x0
    Sub b b' op op' x0 -> Sub b b' (_replaceVar op vm) (_replaceVar op' vm) x0
    Mul b b' op op' x0 -> Mul b b' (_replaceVar op vm) (_replaceVar op' vm) x0
    SDiv b op op' x0 -> SDiv b (_replaceVar op vm) (_replaceVar op' vm) x0
    Or op op' x0 -> Or (_replaceVar op vm) (_replaceVar op' vm) x0
    And op op' x0 -> And (_replaceVar op vm) (_replaceVar op' vm) x0
    Xor op op' x0 ->  Xor (_replaceVar op vm) (_replaceVar op' vm) x0
    BitCast op ty x0 -> BitCast (_replaceVar op vm) ty x0
    Phi ty x0 x1 -> Phi ty (Prelude.map (\(op, n) -> (_replaceVar op vm, n)) x0) x1
    Call m_tck cc pas e x0 es x1 -> Call m_tck cc pas e (Prelude.map (\(op, t) -> (_replaceVar op vm, t)) x0) es x1
    _ -> instr

buildCFG :: [BasicBlock] -> IO(Map Name [Name])
buildCFG blocks = do
    let block_map = fromList $ Prelude.map (\(BasicBlock n _ _) -> (n, [])) blocks
    let block_succs = Prelude.map (\(BasicBlock n _ t) -> (n, getSuccessors t)) blocks
    let block_succ_pairs_ = concatMap (\(n, succs) -> Prelude.map (\s -> (n, s)) succs) block_succs
    -- to each successor, add the predecessor 
    let block_map' = Prelude.foldl (\m (p, s) -> insertWith (++) s [p] m) block_map block_succ_pairs_
    return block_map'
getSuccessorsBlock :: BasicBlock -> [Name]
getSuccessorsBlock (BasicBlock _ _ t ) = getSuccessors t
getSuccessors :: Named Terminator -> [Name]
getSuccessors (Do (Ret _ _)) = []
getSuccessors (Do (CondBr _ t f _)) = [t, f]
getSuccessors (Do (Br t _)) = [t]
getSuccessors _ = []



class RemoveDeadBlocks a where
    removeDeadBlocks :: a -> IO a
instance RemoveDeadBlocks Module where
    removeDeadBlocks llvm = do
        removeDeadBlocks_topdefs <- mapM removeDeadBlocks (moduleDefinitions llvm)
        return llvm { moduleDefinitions = removeDeadBlocks_topdefs }
instance RemoveDeadBlocks Definition where
    removeDeadBlocks (GlobalDefinition f) = GlobalDefinition <$> removeDeadBlocks f
    removeDeadBlocks d = return d
instance RemoveDeadBlocks Global where
    removeDeadBlocks f@(Function {..}) = do
        cf <- buildCFG basicBlocks
        return f { basicBlocks = remove basicBlocks cf }
        where

            remove :: [BasicBlock] -> (Map Name [Name]) -> [BasicBlock]
            remove [] _ = []
            -- if not entry and no predecessors, remove block
            remove (b:bs) cf = case Map.lookup (nameOfBlock b) cf of
                Just [] -> if nameOfBlock b == mkName "entry" then b : remove bs cf else remove bs cf
                _ -> b : remove bs cf

    removeDeadBlocks f = return f

nameOfBlock :: BasicBlock -> Name
nameOfBlock (BasicBlock n _ _) = n

data InlineEnv = InlineEnv {
    _empty_functions :: [(Name, Definition)],
    _label_map :: Map Name Name,
    _internal_label_map :: Map Name Name,
    _arg_map :: Map Name Operand,
    _next_label :: Int,
    _next_var_label :: Int

}
getInlineEnv :: [(Name, Definition)] -> InlineEnv
getInlineEnv empty_functions = InlineEnv empty_functions Map.empty Map.empty  Map.empty 0 0

type InlineMonad a = StateT InlineEnv  IO a
class Inline a where
    inline :: a  -> InlineMonad a


makeLenses ''InlineEnv
instance Inline Module where
    inline llvm  = do
        -- gcse_topdefs <- mapM inline (moduleDefinitions llvm) empty_functions
        gcse_topdefs <- mapM inline (moduleDefinitions llvm)
        return llvm { moduleDefinitions = gcse_topdefs }

instance Inline Definition where
    inline (GlobalDefinition f) = GlobalDefinition <$> inline f
    inline d = return d

instance Inline Global where
    inline f@(Function {..})  = do
        env <- get
        gcse_blocks <- mapM  _inlineBasicBlock  basicBlocks
        new_phi_blocks <- mapM _replacePhi basicBlocks
        put env
        gcse_blocks' <- mapM _inlineBasicBlock new_phi_blocks

        return f { basicBlocks = concat gcse_blocks' }
    inline f  = return f

_replacePhi :: BasicBlock  -> InlineMonad BasicBlock
_replacePhi (BasicBlock name instrs term) = do
    phiMap <- gets $ view label_map
    let instrs' = Prelude.map (replacePhiInstr phiMap) instrs
    return $ BasicBlock name instrs' term

replacePhiInstr :: Map Name Name -> Named Instruction -> Named Instruction
replacePhiInstr phiMap (na := Phi ty ops x0) = na := Phi ty (Prelude.map (\(op, n) -> (op, replacePhiN n phiMap)) ops) x0
    where
        replacePhiN :: Name -> Map Name Name -> Name
        replacePhiN n phiMap = case Map.lookup n phiMap of
            Just n' -> n'
            Nothing -> n

replacePhiInstr _ instr = instr

_inlineBasicBlock :: BasicBlock -> InlineMonad [BasicBlock]
_inlineBasicBlock block = do
        let (BasicBlock name instrs term) = block
        blocks' <- inlineInstrs instrs  [BasicBlock name [] (Do (Ret Nothing []))] term
        let (BasicBlock name' instrs' term') = last blocks'
        modify $ label_map %~ Map.insert name name'
        return blocks'
        -- instrs' <- mapM (\instr -> _inlineInstr instr empty_functions) instrs
        -- _composeBlocks instrs' [(BasicBlock name [] (Do (Ret Nothing [])))] term 0
-- _composeBlocks :: [Either (Named Instruction) [BasicBlock]] -> [BasicBlock]  -> Named Terminator -> Int  -> IO [BasicBlock]
-- _composeBlocks [] blocks term _  = pure $ (init blocks) ++ [BasicBlock name' instrs' term]
--     where (BasicBlock name' instrs' term') = last blocks
-- _composeBlocks _ _ _ _ = undefined
-- _composeBlocks V
--     let (BasicBlock name' instrs' term') = blocks !! i
--     let instrs'' = instrs' ++ [instr]
--     if is_return then return $ blocks ++ [BasicBlock name' instrs'' term'] else do
--         _composeBlocks insts blocks name term (i + 1) is_return
inlineInstrs :: [Named Instruction]  -> [BasicBlock] -> Named Terminator -> InlineMonad [BasicBlock]
inlineInstrs []  blocks term = pure $ (init blocks) ++ [BasicBlock name' instrs' term]
    where (BasicBlock name' instrs' term') = last blocks

inlineInstrs (instr:instrs) blocks term  = do
    empty_functions <- gets $ view empty_functions
    let call = case instr of
            na:= f@Call {function= fname, ..} ->
                if getFName fname `elem` (Prelude.map (\(n, _) -> n) empty_functions) then Just (f, getFName fname, na) else Nothing
            Do f@Call {function= fname, ..} ->
                if (getFName fname ) `elem` (Prelude.map (\(n, _) -> n) empty_functions) then Just (f, getFName fname, mkName "") else Nothing
            _ -> Nothing

    case call of
        Nothing -> do
            let  (BasicBlock name' instrs' term') = last blocks
            inlineInstrs instrs (init blocks ++ [BasicBlock name' (instrs' ++ [instr]) term']) term
        Just f -> do
            let ( instr, name, var) = f
            let (n, def) = fromJust $ Data.List.find (\(n, _) -> n == name) empty_functions
            let  (BasicBlock name' instrs' term') = last blocks
            bName <- getNewBlockName
            env <- get 
            _ <- inlineFunction instr def bName var
            env' <- get
            put env 
            modify $ arg_map .~ env' ^. arg_map 
            blocks' <- inlineFunction instr def bName var
            modify $ arg_map .~ env ^. arg_map 
            modify $ next_var_label .~ env' ^. next_var_label
            inlineInstrs instrs  ((init blocks) ++ [BasicBlock name' instrs' (Do (Br bName []))] ++ blocks') term

getNewBlockName :: InlineMonad Name
getNewBlockName = do
    modify $ next_label %~ (+1)
    i <- gets $ view next_label
    return $ mkName $ "IB_" ++ show i

getFName :: CallableOperand -> Name
getFName (Right (ConstantOperand (C.GlobalReference _ nm))) = nm
getFName _ = mkName ""

nameToString :: Name -> String
nameToString (Name s) = unpack . fromShort $ s
nameToString _ = "NOTNAME"

inlineFunction :: Instruction -> Definition -> Name -> Name  -> InlineMonad [BasicBlock]
inlineFunction (Call {callingConvention = cc1, functionAttributes = fa1, metadata = md1, returnAttributes = ra1, ..})
               def@(GlobalDefinition Function {callingConvention = cc2, functionAttributes = fa2, metadata = md2, returnAttributes = ra2, ..})
               newName
               var  = do
    let (BasicBlock name instrs term) = head basicBlocks

    modify $ internal_label_map %~ Map.insert name newName
    later_blocks <- mapM getNewName (tail basicBlocks)
    let blocks = (BasicBlock newName instrs term) : later_blocks
-- Step 1: Get the list of function arguments as Operands
    let funcArgs = Prelude.map (\(Parameter paramType paramName _) -> paramName) $ fst parameters
    let callArgs = Prelude.map fst arguments
    -- Step 2: Create the argMap by zipping function arguments with call arguments
    let argMap = Map.fromList $ zip funcArgs callArgs
    -- add argmap to the state
    modify $ arg_map %~ Map.union argMap
    outBlockName <- getNewBlockName

    r <- mapM (replaceBlockArgs  outBlockName var) blocks
    let (phiargs, blocks') =  unzip r
    let phiargs' = Prelude.filter (\(n, o) -> isJust o) phiargs
    argMap <- gets $ view arg_map
    let  replaceName var = getArg var argMap
    let phiargs'' = Prelude.map (\(n, Just o) -> (replaceName o, n)) phiargs'
    let listMap = Map.toList argMap

    let phiCall = if returnType == void then [] else if length phiargs'' == 1 then [var := BitCast (fst $ head phiargs'') returnType []]
        else  [var := Phi returnType phiargs'' []  ]
    let final_block =  BasicBlock outBlockName phiCall  term
    pure $ blocks' ++ [final_block]
inlineFunction _ _ _ _ = undefined

getStrings :: [(Operand, Operand)] -> [(String, String)]
getStrings [] = []
getStrings ((LocalReference _ n, LocalReference _ n'):xs) = (nameToString n, nameToString n') : getStrings xs
getStrings _ = []
getNewName :: BasicBlock -> InlineMonad BasicBlock
getNewName (BasicBlock name i t) = do
    next_label <- getNewBlockName
    modify $ internal_label_map %~ Map.insert name next_label
    return $ BasicBlock next_label i t

replaceBlockArgs :: Name -> Name -> BasicBlock -> InlineMonad ((Name, Maybe Operand), BasicBlock)
replaceBlockArgs  outBlockName varName (BasicBlock name instrs term) = do
    instrs' <-   mapM (replaceInstrArgs name) instrs

    (op, term') <-  replaceTermArgs outBlockName term name
    pure (op, BasicBlock name (concat instrs') term')

replaceInstrArgs :: Name -> Named Instruction -> InlineMonad ([Named Instruction])
replaceInstrArgs bname  (na := instr) = do
    argMap <- gets $ view arg_map
    case Map.lookup na argMap of
        Just na' -> do
            r <- replaceArgs bname instr
            pure $ [getN na' := r]
        Nothing -> do
            na' <- getNewVarName
            modify $ arg_map %~ Map.insert ( na) (LocalReference (fromJust $ instructionType instr) na')
            r <- replaceArgs bname instr
            pure $ [na' := r]
    where getN :: Operand -> Name
          getN (LocalReference _ n) = n
          getN _ = mkName ""

replaceInstrArgs bname  (Do instr) = do
    r <- replaceArgs bname instr
    pure $ [Do r]

instructionType :: Instruction -> Maybe Type
instructionType instr = case instr of
    Add {..}          -> Just operandType
    Sub {..}          -> Just operandType
    Mul {..}          -> Just operandType
    UDiv {..}         -> Just operandType
    SDiv {..}         -> Just operandType
    FDiv {..}         -> Just operandType
    And {..}          -> Just operandType
    Or {..}           -> Just operandType
    Phi {..}          -> Just type'
    Xor {..}          -> Just operandType
    FAdd {..}         -> Just operandType
    FSub {..}         -> Just operandType
    FMul {..}         -> Just operandType
    SRem {..}         -> Just operandType
    ICmp {..}         -> Just i1       -- Comparison instructions return `i1`
    Load {..}         -> case typeOf address of
                            PointerType elemType _ -> Just elemType
                            _ -> Nothing
    Store {..}        -> Nothing       -- Store does not produce a result
    Call {..}         ->  case typeOf function of
                            (FunctionType retType _ _) -> Just retType
                            PointerType (FunctionType retType _ _) _ -> Just retType
                            _ -> Just $ typeOf function
    BitCast {..}      -> Just type'
    GetElementPtr {..} -> case typeOf address of
                            PointerType elemType _ -> Just elemType
                            _ -> Nothing
    _ -> Nothing
  where
    operandType = typeOf $ operand0 instr
getNewVarName :: InlineMonad Name
getNewVarName = do
    modify $ next_var_label %~ (+1)
    i <- gets $ view next_var_label
    return $ mkName $ "_iv" ++ show i

replaceArgs :: Name -> Instruction -> InlineMonad Instruction
replaceArgs bName instr = do
    argMap <- gets $ view arg_map
    labelMap <- gets $ view internal_label_map
    let replaceArg = (\op -> getArg op argMap)
    let replaceBlockName = (\n -> fromMaybe n (Map.lookup n labelMap))
    _ <- case instr of 
        Load b op ma wo x0 -> do 
            -- liftIO $ print $ Data.Text.Lazy.unpack (ppll instr) ++ " -> "  ++ Data.Text.Lazy.unpack (ppll (Load b (replaceArg op) ma wo x0))
            -- liftIO $ print ("argMap: " ++ show (Prelude.map (\(k, v) -> (nameToString k, nameToString $ getN v)) (Map.toAscList argMap)))
            pure ()
        _ -> pure ()
    pure $ case instr of
        Add b b' op op' x0 -> Add b b' (replaceArg op) (replaceArg op') x0
        Sub b b' op op' x0 -> Sub b b' (replaceArg op) (replaceArg op') x0
        Mul b b' op op' x0 -> Mul b b' (replaceArg op) (replaceArg op') x0
        FMul fmf op op' x0 -> FMul fmf (replaceArg op) (replaceArg op') x0
        UDiv b op op' x0 -> UDiv b (replaceArg op) (replaceArg op') x0
        SDiv b op op' x0 -> SDiv b (replaceArg op) (replaceArg op') x0
        FDiv fmf op op' x0 -> FDiv fmf (replaceArg op) (replaceArg op') x0
        And op op' x0 -> And (replaceArg op) (replaceArg op') x0
        Or op op' x0 -> Or (replaceArg op) (replaceArg op') x0
        Xor op op' x0 -> Xor (replaceArg op) (replaceArg op') x0
        Alloca ty m_op wo x0 -> Alloca ty (fmap replaceArg m_op) wo x0
        Load b op ma wo x0 -> Load b (replaceArg op) ma wo x0
        Store b op op' ma wo x0 -> Store b (replaceArg op) (replaceArg op') ma wo x0
        GetElementPtr b op ops x0 -> GetElementPtr b (replaceArg op) (Prelude.map replaceArg ops) x0
        IntToPtr op ty x0 -> IntToPtr (replaceArg op) ty x0
        BitCast op ty x0 -> BitCast (replaceArg op) ty x0
        AddrSpaceCast op ty x0 -> AddrSpaceCast (replaceArg op) ty x0
        ICmp ip op op' x0 -> ICmp ip (replaceArg op) (replaceArg op') x0
        FCmp fpp op op' x0 -> FCmp fpp (replaceArg op) (replaceArg op') x0
        Phi ty x0 x1 -> do
            Phi ty (Prelude.map (\(op, n) -> ((replaceArg op), replaceBlockName n)) x0) x1
        Call m_tck cc pas e x0 es x1 -> Call m_tck cc pas e (Prelude.map (\(op, t) -> (replaceArg op, t)) x0) es x1
        VAArg op ty x0 -> VAArg (replaceArg op) ty x0
        SRem  op op' x0 -> SRem  (replaceArg op) (replaceArg op') x0
        _ -> instr

getN  :: Operand -> Maybe Name
getN (LocalReference _ n) = Just n
getN (ConstantOperand (C.GlobalReference _ n)) = Just n
getN _ = Nothing

getArg :: Operand -> Map Name Operand -> Operand
getArg op argMap = do 
    let n = getN op
    case n of 
        Just n' ->
            case Map.lookup n' argMap of
                Just n'' -> replaceName op n''
                Nothing -> op
        Nothing -> op
    where  
        replaceName ::  Operand ->  Operand -> Operand 
        replaceName ( LocalReference t _) (LocalReference _ n) = LocalReference t n
        replaceName (ConstantOperand (C.GlobalReference t _)) (ConstantOperand (C.GlobalReference _ n)) = ConstantOperand (C.GlobalReference t n)
        replaceName op1 op2 = op2

replaceTermArgs :: Name -> Named Terminator  -> Name -> InlineMonad( (Name, Maybe Operand), Named Terminator)
replaceTermArgs  outBlockName term currentBlockN = do
    argMap <- gets $ view arg_map
    let replaceArg op = getArg op argMap
    labelMap <- gets $ view internal_label_map
    let replaceBlockName n = fromMaybe n (Map.lookup n labelMap)
    case term of
        Do ter -> case ter of
            -- Ret m_op x0  -> ((currentBlockN, m_op), Do (Br outBlockName x0))
            Ret m_op x0 -> case m_op of
                Just op -> pure ((currentBlockN, Just (replaceArg op)), Do (Br outBlockName x0))
                Nothing -> pure ((currentBlockN, Nothing), Do (Br outBlockName x0))
            CondBr op na na' x0 -> pure ((currentBlockN, Nothing), Do (CondBr (replaceArg op) (replaceBlockName na) (replaceBlockName na') x0))
            Br na x0 -> pure ((currentBlockN, Nothing), Do (Br (replaceBlockName na) x0))
            _ -> pure ( (currentBlockN, Nothing), term)
        _ -> pure ( (currentBlockN, Nothing), term)

    -- remove all arguments by new arguments, and replace all return values by the instruction



-- _inlineInstr :: Named Instruction -> [(Name, Definition)] -> IO(Either (Named Instruction) [BasicBlock])
-- _inlineInstr instr empty_function = pure $ Left instr
    -- case instr of
    --     na := in' -> do
    --         let in'' = _replaceVarInstr in' empty_function
    --         return $ Left (na := in'')
    --     Do in' -> do
    --         let in'' = _replaceVarInstr in' empty_function
    --         return $ Left (Do in'')

calledFunctions :: Definition -> [Name]
calledFunctions (GlobalDefinition Function {..}) =
  (concatMap callsFromBasicBlock basicBlocks ) ++ (concatMap callsFromTerminator $ Prelude.map (\(BasicBlock _ _ t) -> t) basicBlocks)
  where
    -- Collect calls from each basic block.
    callsFromBasicBlock :: BasicBlock -> [Name]
    callsFromBasicBlock (BasicBlock _ instrs term) =
        Data.Maybe.mapMaybe callsFromNamedInstr instrs
        ++ callsFromTerminator term

    -- Examine a single named instruction to see if it's a Call.
    callsFromNamedInstr :: Named Instruction -> Maybe Name
    callsFromNamedInstr (_ := instr) =
      case instr of
        Call { function = f } -> extractGlobalName f
        _                     -> Nothing
    callsFromNamedInstr (Do instr) =
      case instr of
        Call { function = f } -> extractGlobalName f
        _                     -> Nothing

    -- The terminator might be Invoke or something else.
    callsFromTerminator :: Named Terminator -> [Name]
    callsFromTerminator _ = []

    -- Extract the 'Name' if the operand is a direct reference (GlobalReference).
    extractGlobalName :: CallableOperand -> Maybe Name
    extractGlobalName (Right (ConstantOperand (C.GlobalReference _ nm))) = Just nm
    extractGlobalName _                                                = Nothing

    -- extractGlobalName (ConstantOperand (C.GlobalReference _ nm)) = Just nm
    -- extractGlobalName _                                          = Nothing

calledFunctions _ = []
getEmptyFunctions :: Module -> IO [(Name, Definition)]
getEmptyFunctions mod = pure $ Data.Maybe.mapMaybe isEmptyFun (moduleDefinitions mod)
  where
    -- Check if a definition is a Function that calls no other functions.
    -- If so, return Just (the function name, the definition), otherwise Nothing.
    isEmptyFun :: Definition -> Maybe (Name, Definition)
    isEmptyFun def@(GlobalDefinition fn@Function{..}) = do

      let calls = calledFunctions def
      let predefined_names = Prelude.map (\(n, _) -> n) astPredifinedFunctions
      let not_predifined_calls = Prelude.filter (\c -> notElem c predefined_names) calls
      if  Prelude.null not_predifined_calls && notElem name predefined_names && noStructsArgs parameters && nameDoesntStartWith name "_"
           then Just (name, def)
           else Nothing
    isEmptyFun _ = Nothing

nameDoesntStartWith :: Name -> String -> Bool
nameDoesntStartWith  s prefix = not $ prefix `Data.List.isPrefixOf` nameToString s

noStructsArgs :: ([Parameter], Bool) -> Bool
noStructsArgs (params, _) = Prelude.all (\(Parameter ty _ _) -> not $ isStructType ty) params

isStructType :: Type -> Bool
isStructType (PointerType (NamedTypeReference _) _) = True
isStructType (StructureType {..}) = True
isStructType _ = False
isStructType _ = False

_inline ::Module -> InlineMonad Module
_inline llvm = do
    llvm' <- inline llvm
    if llvm == llvm' then return llvm' else _inline llvm'
    -- return llvm'

optimize :: Module -> IO Module
optimize llvm = do

    llvm' <- removeDeadBlocks llvm
    empty_functions <- getEmptyFunctions llvm'
    -- liftIO $ print $ "empty functions: " ++ show (Prelude.map (\(n, _) -> n) empty_functions)
    llvm'' <- evalStateT (_inline llvm') (getInlineEnv empty_functions)
    -- llvm''' <- gcse llvm''
    -- return llvm
    return llvm''
