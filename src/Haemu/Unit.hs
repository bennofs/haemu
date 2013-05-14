module Haemu.Unit
  ( Unit(..)
  -- * Re-exported modules that are often needed in the implementation of Units
  , module X
  ) where

import           Control.Monad.Exception as X
import qualified Data.Vector.Unboxed     as V
import           Haemu.Instruction       as X
import           Haemu.Monad             as X
import           Haemu.Types             as X

-- | A unit can run instructions. It provides a function that runs opcodes in the Haemu monad.
-- Units do not share the opcode namespace, they can all have there individual opcodes. (for example,
-- there can be multiple units that have an opcode 0).
data Unit l m = Unit
  { runInstruction :: Opcode -> Dataflags -> V.Vector MemoryByte
                   -> EMT l (HaemuM m Register MemoryByte) ()
  }
