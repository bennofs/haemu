{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haemu.Monad
  (
  -- * Running Haemu actions
    HaemuM
  , runHaemuM
  , HaemuState(..)
  , registers
  , memory
  -- * Basic Haemu actions
  , writeMemory
  , readMemory
  , writeRegister
  , readRegister
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Array.ST        (MArray, STUArray, readArray, thaw,
                                       writeArray)
import           Data.Array.Unboxed
import           Data.Array.Unsafe    (unsafeFreeze)

-- | Holds the state for the Haemu monad. The type arguments are the register store type (r) and
-- the memory store type (m).
data HaemuState r m = HaemuState
  { _registers :: r -- ^ The state of the registers
  , _memory    :: m -- ^ The state of the memory
  }

deriving instance (Show r, Show m) => Show (HaemuState r m)
deriving instance (Eq r, Eq m) => Eq (HaemuState r m)

-- Create lenses for our state data type
makeLenses ''HaemuState

-- | The haemu monad. The haemu monad manages a state of the registers and the memory. The type
-- arguments are a phantom data type s so that references to the memory or registers can't escape
-- the monad, the register type r, address type a, memory value type v and the type b of the value
-- in the monad.
type HaemuM s r a v b = ReaderT (HaemuState (STUArray s Int r) (STUArray s a v)) (ST s) b

-- | Convert a HaemuState with immutable arrays to a state with mutable arrays.
thawState :: (Ix i, Ix j, Applicative f, MArray a' e f, MArray b' d f, IArray a e, IArray b d)
          => HaemuState (a i e) (b j d) -> f (HaemuState (a' i e) (b' j d))
thawState (HaemuState reg mem) = HaemuState <$> thaw reg <*> thaw mem

-- | Convert a HaemuState of mutable arrays to a state with immutable arrays.
unsafeFreezeState :: (Ix i, Ix j, Applicative f, MArray a e f, MArray b d f, IArray a' e, IArray b' d)
                  => HaemuState (a i e) (b j d) -> f (HaemuState (a' i e) (b' j d))
unsafeFreezeState (HaemuState reg mem) = HaemuState <$> unsafeFreeze reg <*> unsafeFreeze mem

-- | Run a Haemu computation in a ST monad, using the supplied 'HaemuState'.
runHaemuM :: (Ix a, MArray (STUArray s) r (ST s), MArray (STUArray s) v (ST s), IArray c r, IArray c v)
          => HaemuM s r a v b                       -- ^ The 'HaemuM' computation to run
          -> HaemuState (c Int r) (c a v)           -- ^ The state to start with
          -> ST s (b, HaemuState (c Int r) (c a v)) -- ^ A ST action returning the value of the
                                                   --   'HaemuM' computation and the update State.
runHaemuM m s = do
  s' <- thawState s
  r <- runReaderT m s'
  sr <- unsafeFreezeState s'
  return (r, sr)

-- | @writeMemory pos val@ stores the value @val@ at position @pos@ in the memory. The current value
-- will be overwritten.
writeMemory :: (Ix a, MArray (STUArray s) v (ST s)) => a -> v -> HaemuM s r a v ()
writeMemory pos val = do
  mem <- view memory
  lift $ writeArray mem pos val

-- | @readMemory pos@ returns the value currently stored at position pos in the memory.
readMemory :: (Ix a, MArray (STUArray s) v (ST s)) => a -> HaemuM s r a v v
readMemory pos = do
  mem <- view memory
  lift $ readArray mem pos

-- | @writeRegister reg val@ writes value val in register reg. Any previous value will be overwritten.
writeRegister :: (MArray (STUArray s) r (ST s)) => Int -> r -> HaemuM s r a v ()
writeRegister reg val = do
  regs <- view registers
  lift $ writeArray regs reg val

-- | @readRegister reg@ returns the value of register reg.
readRegister :: (MArray (STUArray s) r (ST s)) => Int -> HaemuM s r a v r
readRegister reg = do
  regs <- view registers
  lift $ readArray regs reg
