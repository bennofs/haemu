{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haemu.Monad
  (
  -- * Types
    HaemuM
  , HaemuState(..)
  , ImmutableHaemuState
  , MutableHaemuState
  , MVectorGetting
  -- * Running Haemu actions
  , runHaemuM
  , execHaemuM
  , evalHaemuM
  , registers
  , memory
  -- * Constructing the initial state
  -- * Basic Haemu actions
  , write
  , access
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed as V.I
import qualified Data.Vector.Unboxed.Mutable as V.M

-- | Holds the state for the Haemu monad. The type arguments are the register store type (r) and
-- the memory store type (m).
data HaemuState r m = HaemuState
  { _registers :: r -- ^ The state of the registers
  , _memory    :: m -- ^ The state of the memory
  }

-- | Immutable 'HaemuState'
type ImmutableHaemuState r v = HaemuState (V.I.Vector r) (V.I.Vector v)

-- | Mutable 'HaemuState' in the monad m.
type MutableHaemuState m r v = HaemuState (V.M.MVector (PrimState m) r) (V.M.MVector (PrimState m) v)

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

-- | A getting of an mutable vector with values of type v out of the type s in the monad m.
type MVectorGetting m s v = Getting (V.M.MVector (PrimState m) v) s (V.M.MVector (PrimState m) v)

-- | @write l pos val@ stores the value @val@ at position @pos@ in the field viewed by l of the
-- 'HaemuState'.
-- Example: @write memory 3 4@ writes the value 4 at position 3 in the memory.
write :: (MonadTrans t, MonadReader s (t m), PrimMonad m, V.M.Unbox a)
      => MVectorGetting m s a -> Int -> a -> t m ()
write l pos val = do
  x <- view l
  lift $ V.M.write x pos val

-- | @access l pos@ reads the value at @pos@ in the field viewed by the lens @l@.
-- Example: @access registers 4@ reads the value of the register 4.
access :: (MonadTrans t, MonadReader s (t m), PrimMonad m, V.M.Unbox a)
       => MVectorGetting m s a -> Int -> t m a
access l pos = do
  x <- view l
  lift $ V.M.read x pos

-- | @defaultState vreg vmem reg mem@ constructs a 'HaemuState' with @reg@ registers filled
-- with the default value @vreg@ and @mem@ memory places with the default value @vmem@.
defaultState :: (V.M.Unbox r, V.M.Unbox v) => r -> v -> Int -> Int -> ImmutableHaemuState r v
defaultState vreg vmem reg mem = HaemuState (V.I.replicate reg vreg) $ V.I.replicate mem vmem
