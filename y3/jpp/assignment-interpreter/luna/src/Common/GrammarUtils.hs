{-# LANGUAGE PatternSynonyms #-}

module Common.GrammarUtils where

import           Grammar.Abs

pattern NoPos :: BNFC'Position
pattern NoPos = BNFC'NoPosition

showIdent :: Ident -> String
showIdent (Ident name) = name

showPos :: BNFC'Position -> String
showPos (Just (line, column)) =
  concat ["line ", show line, ", column ", show column]
showPos _ = "unknow"

showArg :: Arg -> String
showArg (AArg _ ident typ) = concat [showIdent ident, ": ", show typ]
showArg (AArgVar _ ident typ) =
  concat ["var ", showIdent ident, ": ", showType typ]

showType :: Type -> String
showType (Int _) = "int"
showType (Str _) = "string"
showType (Bool _) = "bool"
showType (Nil _) = "nil"
showType (Fun _ args returnType) =
  concat ["fn(", show args, ")", " -> ", show returnType]
