module Syntax where

import qualified Id as Id
import qualified Type as Type

-- | Data type that represents MinCaml AST.
data Syntax
  = Unit
  | Bool !Bool
  | Int !Int
  | Float !Float
  | Not !Syntax
  | Neg !Syntax
  | Add !Syntax !Syntax
  | Sub !Syntax !Syntax
  | FNeg !Syntax
  | FAdd !Syntax !Syntax
  | FSub !Syntax !Syntax
  | FMul !Syntax !Syntax
  | FDiv !Syntax !Syntax
  | Eq !Syntax !Syntax
  | LE !Syntax !Syntax
  | If !Syntax !Syntax !Syntax
  | Let !Id.Id !Type.Type !Syntax !Syntax
  | Var !Id.Id
  | LetRec !Fundef !Syntax
  | App !Syntax ![Syntax]
  | Tuple ![Syntax]
  | LetTuple ![(Id.Id, Type.Type)] !Syntax !Syntax
  | Array !Syntax !Syntax
  | Get !Syntax !Syntax
  | Put !Syntax !Syntax !Syntax
data Fundef = Fundef { name :: !(Id.Id, Type.Type), args :: ![(Id.Id, Type.Type)], body :: !Syntax }

