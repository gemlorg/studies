module Compiler.IR.Utils where 

import           Common.RTypes

import           Lens.Micro
import qualified Data.Map     as Map

import           Data.Maybe
import           Prelude
import qualified Data.Text as T
import LLVM.AST hiding (Type)
import qualified LLVM.AST.Type as AST
import           Grammar.Abs
-- convertType :: RawType -> AST.Type
-- convertType RTInt = AST.i32

-- import LLVM.AST
import LLVM.AST.Global

-- import LLVM.AST.Type (i32, ptr)

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
predifinedDecl :: [Definition]
predifinedDecl = [printIntDecl, printStringDecl, errorDecl, readIntDecl, readStringDecl, concatStringsDecl, compareStringsDecl]



astPredifinedFunctions :: [(Name, RawType)]
astPredifinedFunctions = map (\(Ident s, t) -> (mkName s, t)) predifinedFunctionsInternal

astFromIdent :: Ident -> Name
astFromIdent (Ident s) = mkName s

astFromArgs:: [Arg] -> ([Parameter], Bool)
astFromArgs args = (map astFromArg args, False)

astFromArg :: Arg -> Parameter
astFromArg (Arg _ t i) = Parameter (astFromType t) (astFromIdent i) []

-- astFromType :: Type -> AST.Type
-- astFromType (Int _) = AST.i32
-- astFromType (Str _) = AST.ptr AST.i8
-- astFromType (Bool _) = AST.i1
-- astFromType (Void _) = AST.void
-- astFromType (Fun _ returnType argumentsTypes) = AST.FunctionType (astFromType returnType) (map astFromType argumentsTypes) False

astFromType :: Type -> AST.Type 
astFromType t = astFromRType (fromType t)

astFromRType :: RawType -> AST.Type
astFromRType RTInt = AST.i32
astFromRType RTString = AST.ptr AST.i8
astFromRType RTBool = AST.i1
astFromRType RTVoid = AST.void
astFromRType (RTFun returnType argsTypes) = AST.FunctionType (astFromRType returnType) (map astFromRType argsTypes) False


astAlloc :: Name -> RawType -> Named Instruction
astAlloc i t =  i := Alloca (astFromRType t) Nothing 0 []



