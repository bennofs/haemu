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

-- | Holds the state for the Haemu monad. The type arguments are the type c of the container which
-- to use for the registers and the memory, the register type r, the address type a and the value
-- type v (Type of the values stored in memory).
data HaemuState c r a v = HaemuState
  { _registers :: c Int r -- ^ A mutable unboxed array storing the register values.
  , _memory    :: c a v      -- ^ The memory is a mutable unboxed array from address to values.
  }

deriving instance (Show (c Int r), Show (c a v)) => Show (HaemuState c r a v)

-- Create lenses for our state data type
makeLenses ''HaemuState

-- | The haemu monad. The haemu monad manages a state of the registers and the memory. The type
-- arguments are a phantom data type s so that references to the memory or registers can't escape
-- the monad, the register type r, address type a, memory value type v and the type b of the value
-- in the monad.
type HaemuM s r a v b = ReaderT (HaemuState (STUArray s) r a v) (ST s) b

-- | Convert a HaemuState with immutable arrays to a state with mutable arrays.
thawState :: (Ix a, Applicative m, IArray c r, IArray c v, MArray d r m, MArray d v m)
          => HaemuState c r a v          -- ^ The state which to convert from
          -> m (HaemuState d r a v)      -- ^ The converted state
thawState (HaemuState reg mem) = HaemuState <$> thaw reg <*> thaw mem

-- | Convert a HaemuState of mutable arrays to a state with immutable arrays.
unsafeFreezeState :: (Ix a, Applicative m, MArray c r m, MArray c v m, IArray d r, IArray d v)
            => HaemuState c r a v        -- ^ The state which to freeze
            -> m (HaemuState d r a v)    -- ^ The frozen state
unsafeFreezeState (HaemuState reg mem) = HaemuState <$> unsafeFreeze reg <*> unsafeFreeze mem

-- | Run a Haemu computation in a ST monad, using the supplied 'HaemuState'.
runHaemuM :: (Ix a, MArray (STUArray t) r (ST t), MArray (STUArray t) v (ST t), IArray c r, IArray c v)
          => HaemuM t r a v b             -- ^ The 'HaemuM' computation to run
          -> HaemuState c r a v           -- ^ The state to start with
          -> ST t (b, HaemuState c r a v) -- ^ A ST action returning the value of the 'HaemuM'
                                         --   computation and the update State.
runHaemuM m s = do
  s' <- thawState s
  r <- runReaderT m s'
  sr <- unsafeFreezeState s'
  return (r, sr)

-- | @writeMemory pos val@ stores the value @val@ at position @pos@ in the memory. The current value
-- will be overwritten.
writeMemory :: (Ix a, MonadReader (HaemuState s b a v) m, MArray s v m) => a -> v -> m ()
writeMemory pos val = do
  mem <- view memory
  writeArray mem pos val

-- | @readMemory pos@ returns the value currently stored at position pos in the memory.
readMemory :: (Ix a, MonadReader (HaemuState s b a v) m, MArray s v m) => a -> m v
readMemory pos = do
  mem <- view memory
  readArray mem pos

-- | @writeRegister reg val@ writes value val in register reg. Any previous value will be overwritten.
writeRegister :: (MonadReader (HaemuState s b a v) m, MArray s b m) => Int -> b -> m ()
writeRegister reg val = do
  regs <- view registers
  writeArray regs reg val

-- | @readRegister reg@ returns the value of register reg.
readRegister :: (MonadReader (HaemuState s b a v) m, MArray s b m) => Int -> m b
readRegister reg = do
  regs <- view registers
  readArray regs reg
