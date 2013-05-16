{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Haemu.Units.Arithmetic
  ( arithmeticUnit
  ) where

import           Control.Monad.Primitive
import           Haemu.Exception
import           Haemu.Unit

arithmeticUnit :: (PrimMonad m, Throws UnknownOpcode l) => Opcode -> Dataflags -> DataBlock -> EMT l (HaemuM m) ()
arithmeticUnit c _ _ = throw $ UnknownOpcode c
