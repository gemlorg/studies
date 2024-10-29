module LLVM.Types where


import           Grammar.Abs


data TracedException e =
  Exception e BNFC'Position

instance Show e => Show (TracedException e) where
  show (Exception e pos) = concat ["error: ", show e, " at ", showPos pos]

showPos :: BNFC'Position -> String
showPos (Just (line, column)) =
  concat ["line ", show line, ", column ", show column]
showPos _ = "unknow"

type CompileException = TracedException CompileException'


data CompileException'
  = DivideByZeroException
  | InvalidStepException
  | NotInScopeException Ident
  deriving (Show)

