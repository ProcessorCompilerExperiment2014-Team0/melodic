module Typing where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import Id
import Syntax
import Type

type TypeEnv = Map Id Type

data TypingError
  = UnifyError Type Type
  | TypingError Syntax Type Type
  | MiscError String Syntax
  deriving (Eq, Show)

type M = CounterT (ExceptT TypingError (StateT UnifyEnv (Reader TypeEnv)))

uniqType :: M Type
uniqType = TVar <$> (("T" ++) <$> (show <$> fresh))

type UnifyEnv = Map String Type


unify :: Type -> Type -> M ()
unify TUnit TUnit = return ()
unify TInt TInt = return ()
unify TBool TBool = return ()
unify TFloat TFloat = return ()
unify (TFun xs x) (TFun ys y) = do
  zipWithM_ unify xs ys
  unify x y
unify x y = throwError (UnifyError x y)

checkBinary :: Type -> Syntax -> Syntax -> M ()
checkBinary ty e1 e2 = do
  typingSub e1 >>= unify ty
  typingSub e2 >>= unify ty
  return ()
-- Type Inference
typingSub :: Syntax -> M Type
typingSub syn = case syn of
  Unit -> return TUnit
  Bool _ -> return TBool
  Int _ -> return TInt
  Float _ -> return TFloat
  Not x -> typingSub x >>= unify TBool >> return TBool
  Neg x -> typingSub x >>= unify TInt >> return TInt
  ArithBin _ e1 e2 -> checkBinary TInt e1 e2 >> return TInt
  FNeg x -> typingSub x >>= unify TFloat >> return TFloat
  FloatBin _ e1 e2 -> checkBinary TFloat e1 e2 >> return TFloat
  Cmp _ e1 e2 -> do
    ty1 <- typingSub e1
    ty2 <- typingSub e2
    unify ty1 ty2
    return ty1
  If e1 e2 e3 -> do
    typingSub e1 >>= unify TBool
    ty2 <- typingSub e2
    ty3 <- typingSub e3
    unify ty2 ty3
    return ty2
  Let x t e1 e2 -> do
    typingSub e1 >>= unify t
    local (Map.insert x t) (typingSub e2)
  Var x -> do
    env <- ask
    case Map.lookup x env of
      Just ty -> return ty
      Nothing -> throwError $ MiscError ("external variable " ++ show x ++ " was not in extenv.") (Var x)
  LetRec (Fundef { name = (x, ty), args = ls, body = e1}) e2 -> do
    newenv <- asks (Map.insert x ty)
    let argsenv = Map.fromList ls `Map.union` newenv
    TFun (map snd ls) <$> (local (const argsenv) (typingSub e1)) >>= unify ty
    local (const newenv) (typingSub e2)
  App e es -> do
    t <- uniqType
    join $ unify <$> typingSub e <*> (flip TFun t <$> mapM typingSub es);
    return t
  Tuple es -> TTuple <$> mapM typingSub es
  
-- | extenv: type info of external functions
typing :: TypeEnv -> Syntax -> Either TypingError Syntax
typing extenv syn = runReader (evalStateT (runExceptT $ runCounterT $ do
    typingSub syn >>= unify TUnit
    return syn
  ) Map.empty) extenv
