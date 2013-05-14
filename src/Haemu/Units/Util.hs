module Haemu.Units.Util
  (
  ) where

import Haemu.Types
import Haemu.Monad
import Haemu.Exception
import Control.Monad.Exception
import Data.Word
import Control.Lens
import Data.Bits.Lens

-- | @interpretArguments f b@ interprets the block of arguments stored in the memory block b using
-- the given flags in f. If bit n in f is set, this means that the argument with index n has to be
-- interpreted as a register. Otherwise, the argument is interpreted as an memory address.
-- Throws InvalidAddress/Register if either the referenced address or register does not exist.
interpretArguments :: (Throws InvalidRegister l, Throws InvalidAddress l)
                   => Dataflags -> DataBlock -> EMT l (HaemuM m Register MemoryByte) Word16
interpretArguments f b = zipWith (uncurry interpretArgument) [0..] $ f ^. bits
  where interpretArgument n False =
