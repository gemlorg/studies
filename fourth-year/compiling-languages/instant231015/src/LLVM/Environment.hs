{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LLVM.Environment where 

import           Grammar.Abs

import           Control.Lens
import qualified Data.Map     as Map

import           Data.Maybe
import           Prelude
import qualified Data.Text as T

type Location = Int

data Env = Env
  { _nextId :: Int,
    _loc :: Map.Map Ident Location
  } deriving (Show, Eq)

-- compile result (Ident, T.Text)
type CompileRes = (Location, [T.Text])

dummyLoc :: Location 
dummyLoc = -1

dummy ::  CompileRes
dummy = (dummyLoc, [])

makeLenses ''Env

emptyEnv :: Env
emptyEnv =  Env {_loc = Map.empty, _nextId = 1}

-- check
isVarUsed :: Ident -> Env -> Bool
isVarUsed i e = isJust $ Map.lookup i (e ^. loc)

-- if var is not used, add it to the environment
updateVar :: Ident -> Env -> Env

updateVar i e = if isVarUsed i e then e else e & loc %~ Map.insert i (e ^. nextId) & nextId %~ (+1)

getLoc :: Ident -> Env -> Location
getLoc i e = fromJust $ Map.lookup i (e ^. loc)



