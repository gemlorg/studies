module Eval.Values where


import          Eval.Environment



showValue :: Value -> String
showValue VNil = "nil"
showValue (VInt i) = show i
showValue (VBool b) = show b
showValue (VStr s) = s
showValue (VFun ident a b c ) = show (VFun ident a b c )
showValue Dummy = "dummy"
showValue Print = "print"

applyToInt :: (Integer -> Integer) -> Value -> Value
applyToInt f (VInt i) = VInt $ f i
applyToInt _ _ = error "applyToInt: not an integer"

valToInt :: Value -> Integer
valToInt (VInt i) = i
valToInt _ = error "valToInt: not an integer"

applyToBool :: (Bool -> Bool) -> Value -> Value
applyToBool f (VBool b) = VBool $ f b
applyToBool _ _ = error "applyToBool: not a boolean"

intOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
intOp f (VInt i1) (VInt i2) = VInt $ f i1 i2
intOp _ _ _ = error "intOp: not an integer"

boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
boolOp f (VBool b1) (VBool b2) = VBool $ f b1 b2
boolOp _ _ _ = error "boolOp: not a boolean"

relOp :: (Integer -> Integer -> Bool) -> Value -> Value -> Value
relOp f (VInt i1) (VInt i2) = VBool $ f i1 i2
relOp _ _ _ = error "relOp: not an integer"
