-- Update Friday May 17

module Expr where 
{-
  Expr describes both the expression types and value types of the mini racket language. 
-}

-- define the operator types
data BoolOp = 
        And 
      | Or 
      | Not
      deriving (Show, Eq)

data MathOp = 
        Add 
      | Sub 
      | Mul 
      | Div 
      | Mod 
      deriving (Show, Eq)

data CompOp = 
          Eq 
        | Lt 
        | Le
        | Gt -- new
        | Ge
        deriving (Show, Eq)

-- define the expression types
data Expr = 
      BoolExpr BoolOp [Expr]
    | NotExpr Expr
    | MathExpr MathOp [Expr] 
    | CompExpr CompOp Expr Expr
    | VarExpr String
    | LiteralExpr Value 
    | LambdaExpr String Expr
    | IfExpr Expr Expr Expr
    | ApplyExpr Expr Expr
    | LetExpr String Expr Expr
    | PairExpr Expr Expr 
    | EmptyExpr 
    deriving (Show, Eq)

-- define the type for values, in the mini racket language
-- these can be integers, bools, pairs, or closures
data Value = 
      IntValue Integer
    | BoolValue Bool 
    | PairValue (Value, Value)
    | ClosureValue String String Expr ValueEnv deriving (Show, Eq)

-- define a value environment, which is a mapping from 
--    names (which are strings) to their associated values
type ValueEnv = [(String, Value)]
