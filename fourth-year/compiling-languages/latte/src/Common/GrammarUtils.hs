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
showArg (Arg _ typ ident ) = concat [showIdent ident, ": ", show typ]

showType :: Type -> String
showType (Int _) = "int"
showType (Str _) = "string"
showType (Bool _) = "boolean"
showType (Void _) = "void"
showType (Fun _ args returnType) =
  concat [show returnType, "(", show args, ")"]
