module Closure where

import Control.Applicative
import Control.Monad.State
import Data.Set (Set, difference, union)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Id
import Type
import Syntax
import KNormal hiding (freeVars)
-- | Type of closure.
data Closure = Closure { entry :: LId, actualFV :: [VId] } deriving (Eq, Show)

-- Expression after closure transformation
data ClosExp
  = CUnit
  | CInt !Int
  | CFloat !Float
  | CNeg !VId
  | CArithBin !ArithBinOp !VId !VId
  | CFNeg !VId
  | CFloatBin !FloatBinOp !VId !VId
  | CIf !CmpOp !VId !VId !ClosExp !ClosExp -- 比較 + 分岐
  | CLet !VId !Type !ClosExp !ClosExp
  | CVar !VId
  | CMakeCls !VId !Type !Closure !ClosExp
  | CAppCls !VId ![VId]
  | CAppDir !LId ![VId]
  | CTuple ![VId]
  | CLetTuple ![(VId, Type)] !VId !ClosExp
  | CGet !VId !VId
  | CPut !VId !VId !VId
  | CExtArray !LId
  deriving (Eq, Show)
data CFundef = CFundef
    { name :: !(Id, Type)
    , args :: ![(Id, Type)]
    , formalFV :: ![(Id, Type)]
    , body :: !ClosExp
    } deriving (Eq, Show)

data Prog = Prog ![CFundef] !ClosExp

freeVars :: ClosExp -> Set VId
freeVars CUnit = Set.empty
freeVars (CInt {}) = Set.empty
freeVars (CFloat {}) = Set.empty
freeVars (CNeg x) = Set.singleton x
freeVars (CArithBin _ x y) = Set.fromList [x, y]
freeVars (CFNeg x) = Set.singleton x
freeVars (CFloatBin _ x y) = Set.fromList [x, y]
freeVars (CIf _ x y e1 e2) = Set.fromList [x, y] `union` freeVars e1 `union` freeVars e2
freeVars (CLet x _t e1 e2) = freeVars e1 `union` Set.delete x (freeVars e2)
freeVars (CVar x) = Set.singleton x
freeVars (CMakeCls x _t (Closure { actualFV = ys }) e) = Set.delete x (Set.fromList ys `union` freeVars e)
freeVars (CAppCls x ls) = Set.fromList (x : ls)
freeVars (CAppDir _ ls) = Set.fromList ls
freeVars (CTuple ls) = Set.fromList ls
freeVars (CLetTuple ls x e) = Set.insert x (freeVars e `difference` Set.fromList (map fst ls))
freeVars (CGet x y) = Set.fromList [x, y]
freeVars (CPut x y z) = Set.fromList [x, y, z]
freeVars (CExtArray {}) = Set.empty

idToVId :: Id -> VId
idToVId (Id x) = VId x

transSub :: Map VId Type -> Set Id -> KNormal -> State [CFundef] ClosExp
transSub env known e = case e of
  KUnit -> return CUnit
  (KInt i) -> return $ CInt i
  (KFloat f) -> return $ CFloat f
  (KNeg (Id x)) -> return $ CNeg (VId x)
  (KArithBin op (Id x) (Id y)) -> return $ CArithBin op (VId x) (VId y)
  (KFNeg (Id x)) -> return $ CFNeg (VId x)
  (KFloatBin op (Id x) (Id y)) -> return $ CFloatBin op (VId x) (VId y)
  KIf cmp (Id x) (Id y) e1 e2 -> CIf cmp (VId x) (VId y) <$> transSub env known e1 <*> transSub env known e2
  KLet (Id x) t e1 e2 -> CLet (VId x) t <$> transSub env known e1 <*> transSub env known e2
  KVar (Id x) -> return $ CVar (VId x)
  KApp (Id x) ys | Set.member (Id x) known -> -- 関数適用の場合
      return $ trace ("directly applying " ++ x ++ "@.") (CAppDir (LId x) (map idToVId ys))
  KApp (Id f) xs -> return $ CAppCls (VId f) (map idToVId xs)
  KTuple xs -> return $ CTuple (map idToVId xs)
  KLetTuple xts (Id y) e1 -> let transXts = map (\(Id x, t) -> (VId x, t)) xts in
      CLetTuple transXts (VId y) <$> transSub (Map.fromList transXts `Map.union` env) known e1
  KGet (Id x) (Id y) -> return $ CGet (VId x) (VId y)
  KPut (Id x) (Id y) (Id z) -> return $ CPut (VId x) (VId y) (VId z)
  KExtArray (Id x) -> return $ CExtArray (LId x)
  KExtFunApp (Id x) ys -> return $ CAppDir (LId ("min_caml_" ++ x)) (map idToVId ys)

trans :: KNormal -> (ClosExp, [CFundef])
trans expr = runState (transSub Map.empty Set.empty expr) []
