module Type where
-- Datatype that represents MinCaml types.
data Type
  = TUnit
  | TBool
  | TInt
  | TFloat
  | TFun ![Type] !Type -- arguments are uncurried
  | TTuple ![Type]
  | TArray !Type
  | TVar !(Maybe Type)

genType :: Type
genType = TVar Nothing

