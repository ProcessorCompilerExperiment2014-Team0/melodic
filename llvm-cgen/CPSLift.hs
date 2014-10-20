{-# LANGUAGE RankNTypes #-}
module CPSLift where

import Control.Monad.Cont (ContT(ContT), runContT)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Identity (IdentityT(IdentityT), runIdentityT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.State (StateT(StateT), runStateT)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer (WriterT(WriterT), runWriterT)
import Control.Monad.RWS (RWST(RWST), runRWST)
import Data.Monoid (Monoid)

class MonadTrans t => CPSLiftable t where
  cpsLift :: (forall r. (a -> m r) -> m r) -> forall r. (a -> t m r) -> t m r

instance CPSLiftable (ExceptT e) where
  cpsLift cps action = ExceptT $ cps (runExceptT . action)

instance CPSLiftable (StateT s) where
  cpsLift cps action = StateT $ \s -> cps (\a -> runStateT (action a) s)

instance CPSLiftable (ReaderT rr) where
  cpsLift cps action = ReaderT $ \rr -> cps (\a -> runReaderT (action a) rr)

instance CPSLiftable (ContT c) where
  cpsLift cps action = ContT $ \ccont ->
    cps $ \a -> runContT (action a) ccont

instance CPSLiftable MaybeT where
  cpsLift cps action = MaybeT $ cps (runMaybeT . action)

instance CPSLiftable IdentityT where
  cpsLift cps action = IdentityT $ cps (runIdentityT . action)

instance Monoid w => CPSLiftable (WriterT w) where
  cpsLift cps action = WriterT $ cps (runWriterT . action)

instance Monoid w => CPSLiftable (RWST r w s) where
  cpsLift cps action = RWST $ \r s -> cps (\a -> runRWST (action a) r s)
