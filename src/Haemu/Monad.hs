{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haemu.Monad
  (
  -- * Types
    HaemuM
  , HaemuState(..)
  , ImmutableHaemuState
  , MutableHaemuState
  -- * Running Haemu actions
  , runHaemuM
  , execHaemuM
  , evalHaemuM
  , registers
  , memory
  -- * Using the Haemu monad
  , access
  , write
  , hperform
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Data.Monoid
import qualified Data.Vector.Unboxed         as V.I
import qualified Data.Vector.Unboxed.Mutable as V.M

-- | Holds the state for the Haemu monad. The type arguments are the register store type (r) and
-- the memory store type (m).
data HaemuState r m = HaemuState
  { _registers :: r -- ^ The state of the registers
  , _memory    :: m -- ^ The state of the memory
  }

-- | We can make HaemuState an instance of Monoid if the register and memory types are monoids by
-- distributing the monoid operations over the product type.
instance (Monoid r, Monoid m) => Monoid (HaemuState r m) where
  mempty = HaemuState mempty mempty
  mappend (HaemuState r m) (HaemuState r' m') = HaemuState (r <> r') (m <> m')

-- | Immutable 'HaemuState'
type ImmutableHaemuState r v = HaemuState (V.I.Vector r) (V.I.Vector v)

-- | Mutable 'HaemuState' in the monad m.
type MutableHaemuState m r v = HaemuState (V.M.MVector (PrimState m) r) (V.M.MVector (PrimState m) v)

-- Those instances are for testing
deriving instance (Show r, Show m) => Show (HaemuState r m)
deriving instance (Eq r, Eq m) => Eq (HaemuState r m)

-- Create lenses for our state data type
makeLenses ''HaemuState

-- | The haemu monad. The haemu monad manages a state of the registers and the memory. The type
-- arguments are a the state monad type m (IO or ST), the register type r and the memory value type
-- v.
type HaemuM m r v = ReaderT (MutableHaemuState m r v) m

-- | Convert an 'ImmutableHaemuState' to a 'MutableHaemuState'
thawState :: (Applicative f, PrimMonad f, V.M.Unbox a, V.M.Unbox b)
          => ImmutableHaemuState a b -> f (MutableHaemuState f a b)
thawState = registers V.I.thaw >=> memory V.I.thaw

-- | Unsafely convert a MutableHaemuState to an 'ImmutableHaemuState'. Make sure the mutable
-- state isn't ever used again after calling this function!
unsafeFreezeState :: (Applicative f, PrimMonad f, V.M.Unbox a, V.M.Unbox b)
                  => MutableHaemuState f a b
                  -> f (ImmutableHaemuState a b)
unsafeFreezeState = registers V.I.unsafeFreeze >=> memory V.I.unsafeFreeze

-- | Given an initial state, run a Haemu computation in the underlying monad, returning the
-- resulting value and state.
runHaemuM :: (Applicative m, PrimMonad m, V.M.Unbox r, V.M.Unbox v)
          => HaemuM m r v a
          -> ImmutableHaemuState r v
          -> m (a, ImmutableHaemuState r v)
runHaemuM m s = do
  s' <- thawState s
  (,) <$> runReaderT m s' <*> unsafeFreezeState s'

-- | Same as 'runHaemuM', but this function only returns the final state and not the value of
-- the computation.
execHaemuM :: (Applicative m, PrimMonad m, V.M.Unbox r, V.M.Unbox v)
           => HaemuM m r v a
           -> ImmutableHaemuState r v
           -> m (ImmutableHaemuState r v)
execHaemuM m s = snd <$> runHaemuM m s

-- | Same as 'runHaemuM', but this function only returns the value of the computation and not the
-- final state.
evalHaemuM :: (Applicative m, PrimMonad m, V.M.Unbox r, V.M.Unbox v)
           => HaemuM m r v a
           -> ImmutableHaemuState r v
           -> m a
evalHaemuM m s = fst <$> runHaemuM m s

-- | This is just a flipped version of Data.Vector.Unboxed.Mutable.read converted to a Getter,
-- renamed to avoid the name clash with Prelude.
access :: (PrimMonad m, V.M.Unbox a) => Int -> Action m (V.M.MVector (PrimState m) a) a
access = act . flip V.M.read

-- | This is just a flipped version of Data.Vector.Unboxed.Mutable.write converted to a Getter.
write :: (PrimMonad m, V.M.Unbox a) => Int -> a -> Action m (V.M.MVector (PrimState m) a) ()
write i a = act $ \v -> V.M.write v i a

-- | Like 'perform' from lens, with the difference that it takes the source value for the lens from
-- reader monad transformer layer around the monad the action runs in.
hperform :: (MonadTrans t, Monad m, MonadReader a (t m)) => Acting m b a t1 b b1 -> t m b
hperform a = ask >>= lift . perform a
