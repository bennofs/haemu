module Haemu.Types
  ( Opcode
  , Optype
  , Dataflags
  , Condition
  , Register
  , Address
  , Word12
  ) where

import Data.Word
import Data.Word.Odd (Word4, OddWord, One, Zero)

-- | The Word12 type synonym in the OddWord package is wrong. I sent a patch, but until
-- it gets applied and uploaded to hackage we have to use this as a workaround.
type Word12 = OddWord Word16 (One (One (Zero (Zero ())))) -- 12 = 1100 in binary

type Optype = Word4
type Opcode = Word12
type Dataflags = Word4
type Condition = Word8
type Register = Word16
type Address = Word16
