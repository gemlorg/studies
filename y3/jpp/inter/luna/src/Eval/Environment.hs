{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Eval.Environment where

import           Grammar.Abs

import           Control.Lens
import qualified Data.Map     as Map

import           Data.Maybe
import           Prelude


returnLabel :: Ident
returnLabel = Ident "return"

printLabel :: Ident
printLabel = Ident "print"


data PState = PState
  { _env   :: Env
  , _store :: Store
  } deriving (Show)

type Location = Int

newtype Env = Env
  { _loc :: Map.Map Ident Location
  } deriving (Show)


data Store = Store
  { _val       :: Map.Map Location Value
  , _lastLocation :: Int
  } deriving (Show)

data Value
    = VNil 
    | VInt Integer
    | VBool Bool
    | VStr  String
    | VFun [Arg] Block Env
    | Dummy
    deriving (Show)

instance Eq Value where
  VNil == VNil = True
  VInt i1 == VInt i2 = i1 == i2
  VBool b1 == VBool b2 = b1 == b2
  VStr s1 == VStr s2 = s1 == s2
  VFun _ _ _ == VFun _ _ _ = False
  Dummy == Dummy = True
  _ == _ = False

showValue :: Value -> String
showValue VNil = "nil"
showValue (VInt i) = show i
showValue (VBool b) = show b
showValue (VStr s) = s
showValue (VFun a b c ) = show (VFun a b c )
showValue Dummy = "dummy"


makeLenses ''Env
makeLenses ''Store
makeLenses ''PState


applyToInt :: (Integer -> Integer) -> Value -> Value
applyToInt f (VInt i) = VInt $ f i
applyToInt _ _ = error "applyToInt: not an integer"

applyToBool :: (Bool -> Bool) -> Value -> Value
applyToBool f (VBool b) = VBool $ f b
applyToBool _ _ = error "applyToBool: not a boolean"

intOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
intOp f (VInt i1) (VInt i2) = VInt $ f i1 i2
intOp _ _ _ = error "intOp: not an integer"

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
boolOp f (VBool b1) (VBool b2) = VBool $ f b1 b2
boolOp _ _ _ = error "boolOp: not a boolean"

relOp :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
relOp f (VInt i1) (VInt i2) = VBool $ f i1 i2
relOp _ _ _ = error "relOp: not an integer"


emptyEnv :: Env
emptyEnv = Env
  { _loc = Map.empty
  }

emptyStore :: Store
emptyStore = Store
  { _val = Map.empty
  , _lastLocation = 0
  }

emptyPState :: PState
emptyPState = PState
  { _env = emptyEnv
  , _store = emptyStore
  }

getEnvLocation :: Ident -> Env -> Location
getEnvLocation name Env{..} = fromJust $ Map.lookup name _loc

getSafeEnvLocation :: Ident -> Env -> Location
getSafeEnvLocation name Env{..} = fromMaybe (-1) $ Map.lookup name _loc

putEnvLocation :: Ident -> Location -> Env -> Env
putEnvLocation name location Env{..} =
  Env{_loc=Map.insert name location _loc}

getStoreValue :: Location -> Store -> Value
getStoreValue location Store{..} = fromJust $ Map.lookup location _val

updateStoreValue :: Location -> Value -> Store -> Store
updateStoreValue location value Store{..} =
  Store {_val=Map.insert location value _val, _lastLocation=_lastLocation}

putStoreValue :: Value -> Store -> (Location, Store)
putStoreValue value Store{..} =
  (newLocation, Store{_val=newStore, _lastLocation=newLocation})

  where
    newLocation = _lastLocation + 1
    newStore = Map.insert newLocation value _val


getValue :: Ident -> PState -> Value
getValue name PState{..} = getStoreValue (getEnvLocation name _env) _store

updateValue :: Ident -> Value -> PState -> PState
updateValue name value PState{..} =
  PState {_env=_env, _store=updateStoreValue (getEnvLocation name _env) value _store}

putValue :: Ident -> Value -> PState -> PState
putValue name value PState{..} =
    PState {_env=newEnv, _store=newStore}

  where
    (newLocation, newStore) = putStoreValue value _store
    newEnv = putEnvLocation name newLocation _env


getLocation :: Ident -> PState -> Location
getLocation name PState{..} = getEnvLocation name _env

getSafeLocation :: Ident -> PState -> Location
getSafeLocation name PState{..} = getSafeEnvLocation name _env

putLocation :: Ident -> Location -> PState -> PState
putLocation name location PState{..} =
  PState {_env=putEnvLocation name location _env, _store=_store}


getEnv :: PState -> Env
getEnv PState{..} = _env
putEnv :: Env -> PState -> PState
putEnv e PState{..} = PState{_env=e, _store=_store}









-- getEnvLocation :: Ident -> Env -> Location
-- getEnvLocation name  =  fromJust . (^. loc . at name) 

-- putEnvLocation :: Ident -> Location -> Env -> Env
-- putEnvLocation name location = over (loc . at name) (const $ Just location)


-- getStoreValue :: Location -> Store -> Value
-- getStoreValue location = fromJust . (^. val . at location)

-- updateStoreValue :: Location -> Value -> Store -> Store
-- updateStoreValue location value = over (val . at location) (const $ Just value)

-- putStoreValue :: Value -> Store -> Store
-- putStoreValue value Store{_val=st, _lastLocation=lastLoc} =
--   Store{_val=newStore, _lastLocation=newLocation}

--   where
--     newLocation = lastLoc + 1
--     newStore = Map.insert newLocation value st

-- getValue :: Ident -> PState -> Value
-- getValue name pstate = getStoreValue (getEnvLocation name (view env pstate)) (view store pstate)
-- -- getValue name EvaluatorPersistence{..} = getStoreValue (getEnvLocation name env) store

-- updateValue :: Ident -> Value -> PState -> PState
-- updateValue name value pstate = over store (updateStoreValue (getEnvLocation name (view env pstate)) value) pstate
-- --   EvaluatorPersistence {env=env, store=updateStoreValue (getEnvLocation name env) value store}

-- -- does it even work?
-- putValue :: Ident -> Value -> PState -> PState
-- putValue name value pstate = over store (putStoreValue value) pstate & over env (putEnvLocation name (view store pstate ^. lastLocation))


printState :: PState -> IO ()
printState pstate = do
  print $ show pstate