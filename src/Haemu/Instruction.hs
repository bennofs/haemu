{-# LANGUAGE TemplateHaskell #-}
module Haemu.Instruction
  ( sliced
  , instruction
  , Instruction(..)
  , datalength
  , dataflags
  , condition
  , optype
  , opcode
  , dataBlock
  , int
  ) where

import Haemu.Types
import Data.Word
import Data.Bits
import qualified Data.Vector.Unboxed as V
import Control.Lens
import Control.Monad


-- | This represents an instruction as it is used throughout the program.
data Instruction = Instruction
  { _optype :: Optype
  , _opcode :: Opcode
  , _dataflags :: Dataflags
  , _condition :: Condition
  , _dataBlock :: V.Vector Word16
  } deriving (Show, Eq)
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
-- Format of vector (if seen as one big chunk of memory):
--   first 4 bits: count of datablocks (must match with the actual length)
--   bits 4 - 8: register / value flags specifing whether the given parameter is a register or a value
--   bits 8 - 16: condition. Used as a guard. If the guard doesn't match, the instruction is not executed.
--   bits 16 - 20: optype. Instructions can have differnent types (like arithmetic or logical)
--   bits 20 - 32: opcode. An per-optype unique indentifier for the instruction.
--   bits 32 - end: Datablocks
-- Warning: There may be at most 15 data blocks in the instruction for this to be a valid prism.
instruction :: Prism' (V.Vector Word16) Instruction
instruction = prism' f t
  where f (Instruction ot oc df c d) = V.cons b1 $ V.cons b2 d
          where b1 = 0 & sliced 0 4 . int .~ l & sliced 4 4 . int .~ df & sliced 8 8 . int .~ c
                b2 = 0 & sliced 0 4 . int .~ ot & sliced 4 12 . int .~ oc
                l = V.length d
        t b = do
          l <- b ^? ix 0 . sliced 0 4 . int
          df <- b ^? ix 0 . sliced 4 4 . int
          c <- b ^? ix 0 . sliced 8 8 . int
          ot <- b ^? ix 1 . sliced 0 4 . int
          oc <- b ^? ix 1 . sliced 4 12 . int
          let d = V.drop 2 b
          guard $ V.length d == l
          return $ Instruction ot oc df c d

-- | An iso converting from one integral type to another one. Warning: This only a valid iso
-- when no overflows or underflows happen.
int :: (Integral a, Integral b) => Iso' a b
int = iso fromIntegral fromIntegral


-- | The length of the data attached to some instruction
datalength :: Getter Instruction Int
datalength = dataBlock . to V.length
