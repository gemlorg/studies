{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}

module Typechecker.Environment where

import           Common.RTypes
import qualified Data.Map      as Map
import           Grammar.Abs
import           Prelude

import           Lens.Micro
import           Lens.Micro.TH


data Env = Env
  { _types      :: Map.Map Ident RawType
  , _returnFlag :: Bool
  , _classFields :: Map.Map (Ident) [(Ident, RawType)]
  , _classMethods :: Map.Map (Ident) [(Ident, RawType)]
  , _currentClass :: Maybe Ident
  , _extMap :: Map.Map Ident Ident
  }


makeLenses ''Env

emptyEnv :: Env
emptyEnv = Env {_types = Map.fromList predifinedFunctions, _returnFlag = False, _classFields = Map.empty, _classMethods = Map.empty, _currentClass = Nothing, _extMap = Map.empty}

updateTypes :: Env -> [(Ident, RawType)] -> Env
updateTypes = foldl (\env (i, t) -> updateType env i t)



updateType :: Env -> Ident -> RawType -> Env
-- updateType Env {..} name newType = Env {_types = Map.insert name newType _types, _returnFlag = _returnFlag}
updateType env name newType = env & types %~ Map.insert name newType



getType :: Env -> Ident -> Maybe RawType
getType Env {..} name =  case _currentClass of 
  Just className -> case Map.lookup className _classFields of
    Just fields -> case lookup name fields of
      Just t -> Just t
      Nothing ->  Map.lookup name _types
    Nothing -> Map.lookup name _types
  Nothing -> Map.lookup name _types
-- getType env name = env ^. (types . at name)

returnStatementOccured :: Env -> Env
-- returnStatementOccured Env {..} = Env {_types = _types, _returnFlag = True}
returnStatementOccured env = env & returnFlag .~ True

hasReturnStatementOccured :: Env -> Bool
-- hasReturnStatementOccured Env {..} = _returnFlag
hasReturnStatementOccured env = env ^. returnFlag

setRetunFlag :: Env -> Bool -> Env
setRetunFlag env flag = env & returnFlag .~ flag

