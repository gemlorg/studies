{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

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

lambdaLabel :: Ident
lambdaLabel = Ident "lambda"

data PState = PState
  { _env   :: Env
  , _store :: Store
  } deriving (Show, Eq)

type Location = Int

newtype Env = Env
  { _loc :: Map.Map Ident Location
  } deriving (Show, Eq)

data Store = Store
  { _val          :: Map.Map Location Value
  , _lastLocation :: Int
  } deriving (Show, Eq)

data Value
  = VNil
  | VInt Integer
  | VBool Bool
  | VStr String
  | VFun Ident [Arg] Block Env
  | Dummy
  | Print
  deriving (Show, Eq)

makeLenses ''Env

makeLenses ''Store

makeLenses ''PState

emptyEnv :: Env
emptyEnv =  Env {_loc = Map.empty}

emptyStore :: Store
emptyStore = Store {_val = Map.empty, _lastLocation = 0}

emptyPState :: PState
emptyPState = putValue returnLabel Dummy PState {_env = emptyEnv, _store = emptyStore}

getEnvLocation :: Ident -> Env -> Location
-- getEnvLocation name Env {..} = fromJust $ Map.lookup name _loc
getEnvLocation name e  = fromJust $ e ^. (loc . at name)

getSafeEnvLocation :: Ident -> Env -> Location
-- getSafeEnvLocation name Env {..} = fromMaybe (-1) $ Map.lookup name _loc
getSafeEnvLocation name e = fromMaybe (-1) $ e ^. (loc . at name)

putEnvLocation :: Ident -> Location -> Env -> Env
-- putEnvLocation name location Env {..} =
--   Env {_loc = Map.insert name location _loc}
putEnvLocation name location e = e & (loc . at name) ?~ location

getStoreValue :: Location -> Store -> Value
-- getStoreValue location Store {..} = fromJust $ Map.lookup location _val
getStoreValue location s = fromJust $ s ^. (val . at location)

updateStoreValue :: Location -> Value -> Store -> Store
-- updateStoreValue location value Store {..} =
--   Store {_val = Map.insert location value _val, _lastLocation = _lastLocation}
updateStoreValue location value s = s & (val . at location) ?~ value

putStoreValue :: Value -> Store -> (Location, Store)
-- putStoreValue value Store {..} =
--   (newLocation, Store {_val = newStore, _lastLocation = newLocation})
--   where
--     newLocation = _lastLocation + 1
--     newStore = Map.insert newLocation value _val
putStoreValue value s =
  (newLocation, (s & val . at newLocation ?~ value) & lastLocation .~ newLocation)
  where
    newLocation = s ^. lastLocation + 1

getValue :: Ident -> PState -> Value
-- getValue name PState {..} =
--   if name == printLabel
--     then Print
--     else getStoreValue (getEnvLocation name _env) _store
getValue name s = if name == printLabel then Print else getStoreValue (getEnvLocation name $ s ^. env) $ s ^. store

updateValue :: Ident -> Value -> PState -> PState
-- updateValue name value PState {..} =
--   PState
--     { _env = _env
--     , _store = updateStoreValue (getEnvLocation name _env) value _store
--     }
updateValue name value s = s & store %~ updateStoreValue (getEnvLocation name $ s ^. env) value

putValue :: Ident -> Value -> PState -> PState
-- putValue name value PState {..} = PState {_env = newEnv, _store = newStore}
--   where
--     (newLocation, newStore) = putStoreValue value _store
--     newEnv = putEnvLocation name newLocation _env
putValue name value s = s & env %~ putEnvLocation name newLocation & store .~ newStore
  where
    (newLocation, newStore) = putStoreValue value $ s ^. store

getLocation :: Ident -> PState -> Location
-- getLocation name PState {..} = getEnvLocation name _env
getLocation name s = getEnvLocation name $ s ^. env

getSafeLocation :: Ident -> PState -> Location
-- getSafeLocation name PState {..} = getSafeEnvLocation name _env
getSafeLocation name s = getSafeEnvLocation name $ s ^. env

putLocation :: Ident -> Location -> PState -> PState
-- putLocation name location PState {..} =
--   PState {_env = putEnvLocation name location _env, _store = _store}
putLocation name location s = s & env %~ putEnvLocation name location





hasReturned :: PState -> Bool
hasReturned pers = Dummy /= getValue returnLabel pers
