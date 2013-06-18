module Haemu.Types
  ( Opcode
  , Optype
  , Dataflags
  , Condition
  , Register
  , Flags
  , Address
  , MemoryByte
  , DataBlock
  , Word12
  ) where

import Data.Word
import Data.Word.Odd
import qualified Data.Vector.Unboxed as V

type Optype = Word4
type Opcode = Word12
type Dataflags = Word4
type Condition = Word8
type Register = Word16
type Flags = Word32
type MemoryByte = Word16
type Address = Word32
type DataBlock = V.Vector MemoryByte
