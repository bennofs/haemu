-- | A DSL for defining new units
module Haemu.Units.Definition
  ( ArgParser
  , arg
  , runArgParser
  , raw
  , address
  , register
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Bits
import qualified Data.Vector.Unboxed as V
import           Haemu.Types
import           Prelude             hiding (drop, take)

-- | An Applicative Functor that consumes a fixed amout of data from an input vector.
data ArgParser t a = ArgParser Int (V.Vector t -> a)

instance Functor (ArgParser t) where
  fmap f (ArgParser i g) = ArgParser i $ f . g

instance (V.Unbox t) => Applicative (ArgParser t) where
  pure = ArgParser 0 . pure
  (ArgParser n f) <*> (ArgParser m g) = ArgParser (n + m) $ \i -> f (V.take n i) $ g (V.take m $ V.drop n i)

-- | @arg n f@ specifies a single argument of length @n@ with the parse function @f@.
arg :: Int -> (V.Vector t -> a) -> ArgParser t a
arg = ArgParser

-- | @runArgParser p i@ executes the ArgParse p on the input i. If the parser fails, this returns Nothing, otherwise
-- it returns Just the value returned by the parser.
runArgParser :: (V.Unbox t) => ArgParser t a -> V.Vector t -> Maybe a
runArgParser (ArgParser n f) i
  | V.length i == n = Just $ f i
  | otherwise = Nothing


-- | @raw n@ just returns n tokens unprocessed.
raw :: Int -> ArgParser t (V.Vector t)
raw = arg ?? id

-- | @address@ parses an address. It has a length of 2.
address :: ArgParser MemoryByte Address
address = arg 2 $ \v -> fromIntegral (v V.! 0) .|. fromIntegral  (v V.! 1) `shiftL` 16

-- | @register df@ parses an register using df as the DataFlags. It has a length of 1.
register :: Dataflags -> ArgParser MemoryByte Register
register = error "TODO"
