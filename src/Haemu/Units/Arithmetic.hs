{-# LANGUAGE FlexibleContexts #-}
module Haemu.Units.Arithmetic
  ( arithmeticUnit
  ) where

import           Control.Monad.Primitive
import           Haemu.Unit
import           Haemu.Exception

arithmeticUnit :: (PrimMonad m, Throws UnknownOpcode l) => Opcode -> Dataflags -> DataBlock
               -> EMT l (HaemuM m Register MemoryByte) ()
arithmeticUnit c _ _ = throw $ UnknownOpcode c
