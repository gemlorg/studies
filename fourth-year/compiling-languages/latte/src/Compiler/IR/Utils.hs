{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
module Compiler.IR.Utils where

import           Common.RTypes


import           Data.Maybe
import           Prelude
import LLVM.AST hiding (Type)
import qualified LLVM.AST.Type as AST
import           Grammar.Abs
import LLVM.AST.Global
import qualified LLVM.AST.Type
import LLVM.AST.AddrSpace


-- Function declarations
printIntDecl :: Definition
printIntDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "printInt"
  , parameters  = ([Parameter AST.i32 (UnName 0) []], False)
  , returnType  = VoidType
  }

printStringDecl :: Definition
printStringDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "printString"
  , parameters  = ([Parameter (AST.ptr AST.i8) (UnName 0) []], False)
  , returnType  = VoidType
  }

errorDecl :: Definition
errorDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "error"
  , parameters  = ([], False)
  , returnType  = VoidType
  }

readIntDecl :: Definition
readIntDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "readInt"
  , parameters  = ([], False)
  , returnType  = AST.i32
  }

readStringDecl :: Definition
readStringDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "readString"
  , parameters  = ([], False)
  , returnType  = AST.ptr AST.i8
  }

concatStringsDecl :: Definition
concatStringsDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "_concatStrings"
  , parameters  = ([Parameter (AST.ptr AST.i8) (UnName 0) [], Parameter (AST.ptr AST.i8) (UnName 1) []], False)
  , returnType  = AST.ptr AST.i8
  }

compareStringsDecl :: Definition
compareStringsDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "_compareStrings"
  , parameters  = ([Parameter (AST.ptr AST.i8) (UnName 0) [], Parameter (AST.ptr AST.i8) (UnName 1) []], False)
  , returnType  = AST.i32
  }
  
mallocDecl :: Definition
mallocDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "_malloc"
  , parameters  = ([Parameter AST.i32 (UnName 0) []], False)
  , returnType  = AST.ptr AST.i8
  }
countDecl :: Definition 
countDecl = GlobalDefinition $ functionDefaults
  { name        = mkName "_count_arr_length"
  , parameters  = ([Parameter (AST.ptr AST.i8) (UnName 0) [], Parameter AST.i32 (UnName 1) []], False)
  , returnType  = AST.i32
  }


predifinedDecl :: [Definition]
predifinedDecl = [printIntDecl, printStringDecl, errorDecl, readIntDecl, readStringDecl, concatStringsDecl, compareStringsDecl, mallocDecl, countDecl]

ioDecls :: [Definition]
ioDecls = [printIntDecl, printStringDecl, errorDecl, readIntDecl, readStringDecl]


astPredifinedFunctions :: [(Name, RawType)]
astPredifinedFunctions = map (\(Ident s, t) -> (mkName s, t)) predifinedFunctionsInternal

astFromIdent :: Ident -> Name
astFromIdent (Ident s) = mkName s

astFromArgs:: [Arg] -> [Parameter]
astFromArgs args = map astFromArg args

astFromArg :: Arg -> Parameter
astFromArg (Arg _ t i) = Parameter (astFromType t) (astFromIdent i) []


astFromType :: Type -> AST.Type
astFromType t = astFromRType (fromType t)
arrIdent :: Ident 
arrIdent = Ident "_arr"

astFromRType :: RawType -> AST.Type
astFromRType RTInt = AST.i32
astFromRType RTString = AST.ptr AST.i8
astFromRType RTBool = AST.i1
astFromRType RTVoid = AST.void
astFromRType (RTFun returnType argsTypes) = AST.FunctionType (astFromRType returnType) (map astFromRType argsTypes) False
astFromRType (RTClass ident) = getClassType ident
astFromRType (RTArr _ ) = getClassType arrIdent

getClassType :: Ident -> LLVM.AST.Type.Type
getClassType id = do
  PointerType (NamedTypeReference $ astFromIdent id) (AddrSpace 0)


astAlloc :: Name -> RawType -> Named Instruction
astAlloc i t =  i := Alloca (astFromRType t) Nothing 0 []



