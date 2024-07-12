{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}

module Typecheck.Environment where

import           Common.RTypes
import qualified Data.Map      as Map
import           Grammar.Abs
import           Prelude

import           Control.Lens

data Env = Env
  { _types      :: Map.Map Ident RawType
  , _returnFlag :: Bool
  }

makeLenses ''Env

emptyEnv :: Env
emptyEnv = Env {_types = Map.fromList [printSignature], _returnFlag = False}

updateTypes :: Env -> [(Ident, RawType)] -> Env
updateTypes = foldl (\env (i, t) -> updateType env i t)

updateType :: Env -> Ident -> RawType -> Env
-- updateType Env {..} name newType = Env {_types = Map.insert name newType _types, _returnFlag = _returnFlag}
updateType env name newType = env & ((types . at name) ?~ newType)

getType :: Env -> Ident -> Maybe RawType
-- getType Env {..} name = Map.lookup name _types
getType env name = env ^. (types . at name)

returnStatementOccured :: Env -> Env
-- returnStatementOccured Env {..} = Env {_types = _types, _returnFlag = True}
returnStatementOccured env = env & returnFlag .~ True

hasReturnStatementOccured :: Env -> Bool
-- hasReturnStatementOccured Env {..} = _returnFlag
hasReturnStatementOccured env = env ^. returnFlag