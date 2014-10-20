module Syntax where

-- | Data type that represents MinCaml AST.
data Syntax
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Not Syntax
  | Neg Syntax
  | Add Syntax Syntax
  | Sub Syntax Syntax
  | FNeg Syntax
  | FAdd Syntax Syntax
  | FSub Syntax Syntax
  | FMul Syntax Syntax
  | FDiv Syntax Syntax
  | Eq Syntax Syntax
  | LE Syntax Syntax
  | If Syntax Syntax Syntax
  | Let Id.T Type.T Syntax Syntax
  | Var Id.T
  | LetRec Fundef Syntax
  | App Syntax [Syntax]
  | Tuple [Syntax]
  | LetTuple [(Id.T, Type.T)] Syntax Syntax
  | Array Syntax Syntax
  | Get Syntax Syntax
  | Put Syntax Syntax Syntax
data Fundef = Fundef { name :: (Id.T, Type.T), args :: [(Id.T, Type.T)], body :: Syntax }

