-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Grammar.Abs
import Grammar.Lex

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!='     { PT _ (TS _ 1)  }
  '%'      { PT _ (TS _ 2)  }
  '('      { PT _ (TS _ 3)  }
  ')'      { PT _ (TS _ 4)  }
  '*'      { PT _ (TS _ 5)  }
  '+'      { PT _ (TS _ 6)  }
  '++'     { PT _ (TS _ 7)  }
  ','      { PT _ (TS _ 8)  }
  '-'      { PT _ (TS _ 9)  }
  '--'     { PT _ (TS _ 10) }
  '->'     { PT _ (TS _ 11) }
  '/'      { PT _ (TS _ 12) }
  ':'      { PT _ (TS _ 13) }
  ';'      { PT _ (TS _ 14) }
  '<'      { PT _ (TS _ 15) }
  '<='     { PT _ (TS _ 16) }
  '='      { PT _ (TS _ 17) }
  '=='     { PT _ (TS _ 18) }
  '=>'     { PT _ (TS _ 19) }
  '>'      { PT _ (TS _ 20) }
  '>='     { PT _ (TS _ 21) }
  'and'    { PT _ (TS _ 22) }
  'any'    { PT _ (TS _ 23) }
  'bool'   { PT _ (TS _ 24) }
  'else'   { PT _ (TS _ 25) }
  'false'  { PT _ (TS _ 26) }
  'fn'     { PT _ (TS _ 27) }
  'for'    { PT _ (TS _ 28) }
  'if'     { PT _ (TS _ 29) }
  'int'    { PT _ (TS _ 30) }
  'nil'    { PT _ (TS _ 31) }
  'not'    { PT _ (TS _ 32) }
  'or'     { PT _ (TS _ 33) }
  'return' { PT _ (TS _ 34) }
  'string' { PT _ (TS _ 35) }
  'true'   { PT _ (TS _ 36) }
  'var'    { PT _ (TS _ 37) }
  'while'  { PT _ (TS _ 38) }
  '{'      { PT _ (TS _ 39) }
  '}'      { PT _ (TS _ 40) }
  L_Ident  { PT _ (TV _)    }
  L_integ  { PT _ (TI _)    }
  L_quoted { PT _ (TL _)    }

%%

Ident :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Ident) }
Ident  : L_Ident { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Ident (tokenText $1)) }

Integer :: { (Grammar.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (Grammar.Abs.BNFC'Position, String) }
String   : L_quoted { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Program) }
Program
  : ListStmt { (fst $1, Grammar.Abs.PProgram (fst $1) (snd $1)) }

Arg :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Arg) }
Arg
  : Ident ':' Type { (fst $1, Grammar.Abs.AArg (fst $1) (snd $1) (snd $3)) }
  | 'var' Ident ':' Type { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.AArgVar (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }

ListArg :: { (Grammar.Abs.BNFC'Position, [Grammar.Abs.Arg]) }
ListArg
  : {- empty -} { (Grammar.Abs.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Block :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Block) }
Block
  : '{' ListStmt '}' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.BBlock (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (Grammar.Abs.BNFC'Position, [Grammar.Abs.Stmt]) }
ListStmt
  : {- empty -} { (Grammar.Abs.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Stmt) }
Stmt
  : ';' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Empty (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, Grammar.Abs.BStmt (fst $1) (snd $1)) }
  | DeclKind { (fst $1, Grammar.Abs.Decl (fst $1) (snd $1)) }
  | Ident '=' Expr ';' { (fst $1, Grammar.Abs.Ass (fst $1) (snd $1) (snd $3)) }
  | Ident '++' ';' { (fst $1, Grammar.Abs.Incr (fst $1) (snd $1)) }
  | Ident '--' ';' { (fst $1, Grammar.Abs.Decr (fst $1) (snd $1)) }
  | 'return' Expr ';' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Ret (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.VRet (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'if' '(' Expr ')' Stmt { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Cond (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Stmt 'else' Stmt { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.CondElse (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Stmt { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.While (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'for' '(' Stmt ';' Expr ';' Stmt ')' Block { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.For (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7) (snd $9)) }
  | Expr ';' { (fst $1, Grammar.Abs.SExp (fst $1) (snd $1)) }

DeclKind :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.DeclKind) }
DeclKind
  : 'var' Ident ':' Type Item ';' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.VDecl (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $5)) }
  | 'fn' Ident '(' ListArg ')' '->' Type Block { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.FDecl (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $7) (snd $8)) }

Item :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Item) }
Item
  : '=' Expr { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Init (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | {- empty -} { (Grammar.Abs.BNFC'NoPosition, Grammar.Abs.NoInit Grammar.Abs.BNFC'NoPosition) }

ListItem :: { (Grammar.Abs.BNFC'Position, [Grammar.Abs.Item]) }
ListItem
  : Item { (fst $1, (:[]) (snd $1)) }
  | Item ',' ListItem { (fst $1, (:) (snd $1) (snd $3)) }

Type :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Type) }
Type
  : 'nil' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Nil (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'bool' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Bool (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'int' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Int (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Str (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'any' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Any (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'fn' '(' ListType ')' '->' Type { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Fun (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $6)) }

ListType :: { (Grammar.Abs.BNFC'Position, [Grammar.Abs.Type]) }
ListType
  : {- empty -} { (Grammar.Abs.BNFC'NoPosition, []) }
  | Type { (fst $1, (:[]) (snd $1)) }
  | Type ',' ListType { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr6
  : Ident { (fst $1, Grammar.Abs.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, Grammar.Abs.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.ELitTrue (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.ELitFalse (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'nil' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.ENil (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'any' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.EAny (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListExpr ')' { (fst $1, Grammar.Abs.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, Grammar.Abs.EString (fst $1) (snd $1)) }
  | '(' Expr ')' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr5
  : '-' Expr6 { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Neg (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'not' Expr6 { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Not (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, Grammar.Abs.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, Grammar.Abs.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, Grammar.Abs.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr1
  : Expr2 'and' Expr1 { (fst $1, Grammar.Abs.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.Expr) }
Expr
  : Expr1 'or' Expr { (fst $1, Grammar.Abs.EOr (fst $1) (snd $1) (snd $3)) }
  | 'fn' '(' ListArg ')' '=>' Type Block { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.ELambda (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $6) (snd $7)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (Grammar.Abs.BNFC'Position, [Grammar.Abs.Expr]) }
ListExpr
  : {- empty -} { (Grammar.Abs.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.AddOp) }
AddOp
  : '+' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Plus (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Minus (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.MulOp) }
MulOp
  : '*' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Times (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Div (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.Mod (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (Grammar.Abs.BNFC'Position, Grammar.Abs.RelOp) }
RelOp
  : '<' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.LTH (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.LE (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.GTH (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.GE (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.EQU (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1), Grammar.Abs.NE (uncurry Grammar.Abs.BNFC'Position (tokenLineCol $1))) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err Grammar.Abs.Program
pProgram = fmap snd . pProgram_internal
}
