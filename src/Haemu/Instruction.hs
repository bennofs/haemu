{-# LANGUAGE TemplateHaskell #-}
module Haemu.Instruction
  ( sliced
  , instruction
  , datalength
  , dataflags
  , condition
  , optype
  , opcode
  , dataBlock
  ) where

import Data.Word
import Data.Bits
import qualified Data.Vector.Unboxed as V
import Control.Lens
import Control.Monad
import Control.Applicative

-- | An instruction consists of a 16 bits wide control block, a condition and the data.
-- The control block format is:
--  - The first 4 bits contain number of additional data words
--  - The next 4 bits are the register/value flags
--  - The last 8 bits contain the condition
-- The condition guard controls whether the instruction is executed or not, depending on the flags
-- currenty set.
data Instruction = Instruction
  { _controlBlock :: Word16
  , _operationBlock :: Word16
  , _dataBlock :: V.Vector Word16
  } deriving (Show)
makeLenses ''Instruction

-- | A lens for a sliced part of some bits. @sliced n m@ views m bits, starting with bit n (where
-- n = 0 starts with the first bit). The sliced part is readjusted, such that the first bit of the
-- sliced part is the bit at position n in the original.
-- Warning: This is only a valid lens if you don't set it to a value greater than 2^m - 1, in which
-- case all higher bits will be cut off.
sliced :: (Functor f, Bits a, Num a) => Int -> Int -> (a -> f a) -> a -> f a
sliced n m = lens t s
  where mask = (1 `shiftL` m - 1) `shiftL` n
        t x = (x .&. mask) `shiftR` n
        s x v = (x .&. complement mask) .|. ((v `shiftL` n) .&. mask)

-- | Parse / serialize an instruction from / to a vector of Word16.
instruction :: Prism' (V.Vector Word16) Instruction
instruction = prism' f t
  where f (Instruction c g d) = V.cons c $ V.cons g $ d
        t v = Instruction <$> v ^? ix 0 <*> v ^? ix 1 <*> (V.drop 2 v <$ (valid v >>= guard))
        valid v = (== fromIntegral (V.length (V.drop 2 v))) <$> v ^? _head . sliced 0 4

-- | An iso converting from one integral type to another one. Warning: This only a valid iso
-- when no overflows or underflows happen.
int :: (Integral a, Integral b) => Iso' a b
int = iso fromIntegral fromIntegral


-- | The length of the data arguments of an instruction
datalength :: Lens' Instruction Word8
datalength = controlBlock . sliced 0 4 . int

-- | The bits which indicate for each of the maximal 4 input operands of an instruction if this operand is a register or a value
datflags :: Lens' Instruction Word8
dataflags = controlBlock . sliced 4 4 . int

-- | The condition which has to be true for the execution of an instruction
condition :: Lens' Instruction Word8
condition = controlBlock . sliced 8 8 . int

-- | The operationtype of an instruction
optype :: Lens' Instruction Word8
optype = operrationBlock . sliced 0 4 . int

-- | The operationcode of an instruction.
opcode :: Lens' Instruction Word8
opcode = operationBlock . sliced 4 12 . int