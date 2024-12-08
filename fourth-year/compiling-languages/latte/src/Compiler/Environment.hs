{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Compiler.Environment where 

import           Grammar.Abs

import           Lens.Micro
import           Lens.Micro.TH
import qualified Data.Map     as Map

import           Data.Maybe
import           Prelude
import qualified Data.Text as T

type Location = Integer

type ExpMap = Map.Map Expr Location

data Env = Env
  { _nextId :: Location,
    _loc :: Map.Map Ident Location
  } deriving (Show, Eq)

-- compile result (Ident, T.Text)
type CompileRes = T.Text

dummyLoc :: Location 
dummyLoc = -1


makeLenses ''Env

emptyEnv :: Env
emptyEnv =  Env {_loc = Map.empty, _nextId = 1}

isVarUsed :: Ident -> Env -> Bool
isVarUsed i e = isJust $ Map.lookup i (e ^. loc)

updateVar :: Ident -> Env -> Env

updateVar i e = if isVarUsed i e then e else e & loc %~ Map.insert i (e ^. nextId) & nextId %~ (+1)

getLoc :: Ident -> Env -> Location
getLoc i e = fromJust $ Map.lookup i (e ^. loc)

mapSize :: Env -> Int
mapSize e = Map.size (e ^. loc)


