module KNormal where

import Data.Set (Set, difference, union)
import qualified Data.Set as Set

import Id (Id)
import Type (Type)
import Syntax (Syntax)

-- Expression after k-normalization
data KNormal
  = KUnit
  | KInt !Int
  | KFloat !Float
  | KNeg !Id
  | KAdd !Id !Id
  | KSub !Id !Id
  | KFNeg !Id
  | KFAdd !Id !Id
  | KFSub !Id !Id
  | KFMul !Id !Id
  | KFDiv !Id !Id
  | KIfEq !Id !Id !KNormal !KNormal -- 比較 + 分岐
  | KIfLE !Id !Id !KNormal !KNormal -- 比較 + 分岐
  | KLet !Id !Type !KNormal !KNormal
  | KVar !Id
  | KLetRec !KFundef !KNormal
  | KApp !Id ![Id]
  | KTuple ![Id]
  | KLetTuple ![(Id, Type)] !Id !KNormal
  | KGet !Id !Id
  | KPut !Id !Id !Id
  | KExtArray !Id
  | KExtFunApp !Id ![Id]
  deriving (Eq, Show)
data KFundef = KFundef { name :: !(Id, Type), args :: ![(Id, Type)], body :: !KNormal }
  deriving (Eq, Show)

-- | Free Variables in expression(KNormal). Note that external function names are not treated as free.
freeVars :: KNormal -> Set Id
freeVars KUnit = Set.empty
freeVars (KInt {}) = Set.empty
freeVars (KFloat {}) = Set.empty
freeVars (KNeg x) = Set.singleton x
freeVars (KAdd x y) = Set.fromList [x,y]
freeVars (KSub x y) = Set.fromList [x,y]
freeVars (KFNeg x) = Set.singleton x
freeVars (KFAdd x y) = Set.fromList [x,y]
freeVars (KFSub x y) = Set.fromList [x,y]
freeVars (KFMul x y) = Set.fromList [x,y]
freeVars (KFDiv x y) = Set.fromList [x,y]
freeVars (KIfEq x y e1 e2) = Set.fromList [x,y] `union` freeVars e1 `union` freeVars e2
freeVars (KIfLE x y e1 e2) = Set.fromList [x,y] `union` freeVars e1 `union` freeVars e2
freeVars (KLet name_ _ e1 e2) = Set.delete name_ (freeVars e2) `union` freeVars e1
freeVars (KVar x) = Set.singleton x
freeVars (KLetRec (KFundef {name = (x,_), args = a, body = b}) expr) =
  let zs = freeVars b `difference` Set.fromList (map fst a) in
  (zs `union` freeVars expr) `difference` Set.singleton x
freeVars (KApp x y) = Set.singleton x `union` Set.fromList y
freeVars (KTuple xs) = Set.fromList xs
freeVars (KLetTuple decs i e) = Set.singleton i `union` (freeVars e `difference` Set.fromList (map fst decs))
freeVars (KGet x y) = Set.fromList [x,y]
freeVars (KPut x y z) = Set.fromList [x,y,z]
freeVars (KExtArray {}) = Set.empty
freeVars (KExtFunApp _ xs) = Set.fromList xs -- external function name is not free


