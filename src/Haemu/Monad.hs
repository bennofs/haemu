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

-- | Holds the state for the Haemu monad.
data HaemuState v = HaemuState
  { _inspoint  :: Address       -- ^ The state of the instruction pointer
  , _flags     :: Flags         -- ^ The state of the flag register
  , _registers :: v Register    -- ^ The state of the registers
  , _memory    :: v MemmoryByte -- ^ The state of the memory
  }

-- | Immutable 'HaemuState'
type ImmutableHaemuState = HaemuState V.I.Vector

-- | Mutable 'HaemuState' in the monad m.
type MutableHaemuState m = HaemuState V.M.MVector (PrimState m)

-- Those instances are for testing
deriving instance Show HaemuState v
deriving instance Eq HaemuState v

-- Create lenses for our state data type
makeLenses ''HaemuState

-- | The haemu monad. The haemu monad manages a state of the registers and the memory. The type
-- argument is the state monad type m (IO or ST).
type HaemuM m = ReaderT (MutableHaemuState m ) m

-- | Convert an 'ImmutableHaemuState' to a 'MutableHaemuState'
thawState :: (Applicative f, PrimMonad f)
          => ImmutableHaemuState -> f (MutableHaemuState f)
thawState = registers V.I.thaw >=> memory V.I.thaw

-- | Unsafely convert a MutableHaemuState to an 'ImmutableHaemuState'. Make sure the mutable
-- state isn't ever used again after calling this function!
unsafeFreezeState :: (Applicative f, PrimMonad f)
                  => MutableHaemuState f
                  -> f ImmutableHaemuState
unsafeFreezeState = registers V.I.unsafeFreeze >=> memory V.I.unsafeFreeze

-- | Given an initial state, run a Haemu computation in the underlying monad, returning the
-- resulting value and state.
runHaemuM :: (Applicative m, PrimMonad m)
          => HaemuM m a
          -> ImmutableHaemuState
          -> m (a, ImmutableHaemuState)
runHaemuM m s = do
  s' <- thawState s
  (,) <$> runReaderT m s' <*> unsafeFreezeState s'

-- | Same as 'runHaemuM', but this function only returns the final state and not the value of
-- the computation.
execHaemuM :: (Applicative m, PrimMonad m)
           => HaemuM m a
           -> ImmutableHaemuState
           -> m ImmutableHaemuState
execHaemuM m s = snd <$> runHaemuM m s

-- | Same as 'runHaemuM', but this function only returns the value of the computation and not the
-- final state.
evalHaemuM :: (Applicative m, PrimMonad m)
           => HaemuM m a
           -> ImmutableHaemuState
           -> m a
evalHaemuM m s = fst <$> runHaemuM m s

-- | This is just a flipped version of Data.Vector.Unboxed.Mutable.read converted to a Getter,
-- renamed to avoid the name clash with Prelude.
access :: (PrimMonad m, V.M.Unbox a) => Int -> Action m (V.M.MVector (PrimState m) a) a
access = act . flip V.M.read

-- | This is just a flipped version of Data.Vector.Unboxed.Mutable.write converted to a Getter.
write :: (PrimMonad m, V.M.Unbox a) => Int -> a -> Action m (V.M.MVector (PrimState m) a) ()
write i a = act $ \v -> V.M.write v i a

-- | @

-- | Like 'perform' from lens, with the difference that it takes the source value for the lens from
-- reader monad transformer layer around the monad the action runs in.
hperform :: (MonadTrans t, Monad m, MonadReader a (t m)) => Acting m b a t1 b b1 -> t m b
hperform a = ask >>= lift . perform a
