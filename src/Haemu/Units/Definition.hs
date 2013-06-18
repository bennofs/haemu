{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A DSL for defining new units
module Haemu.Units.Definition
  ( Definition
  , ArgType (..)
  , arg
  , argTypes
  , raw
  , address
  , register
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Functor.Product
import           Data.Bits
import qualified Data.Vector.Unboxed as V
import           Haemu.Types
import           Haemu.Monad
import           Prelude             hiding (drop, take)

-- | An Applicative Functor that consumes a fixed amout of data from an input vector.
data ArgParser t a = ArgParser Int (V.Vector t -> a)

instance Functor (ArgParser t) where
  fmap f (ArgParser i g) = ArgParser i $ f . g

instance (V.Unbox t) => Applicative (ArgParser t) where
  pure = ArgParser 0 . pure
  (ArgParser n f) <*> (ArgParser m g) = ArgParser (n + m) $ \i -> f (V.take n i) $ g (V.take m $ V.drop n i)

-- | Enum of possible type of an argument to an instruction
data ArgType = Raw           -- ^ The argument doesn't have a specific type
             | Register      -- ^ The instruction expects a register for this argument
             | MemoryAddress -- ^ The instruction expects a memory address for this argument

-- | Type of a definition of an instruction that takes tokens of type @t@ and returns something of type @a@.
newtype Definition t a = Definition (Product (Const [ArgType]) (ArgParser t) a) 
  deriving (Functor, Applicative)

-- | @arg t n f@ specifies a single argument of length @n@ with the parse function @f@ and type @t@.
arg :: ArgType -> Int -> (V.Vector t -> a) -> Definition t a
arg t n = Definition . Pair (Const [t]) . ArgParser n

-- | @argTypes@ returns the kinds of the args to a given definition.
argTypes :: Definition t a -> [ArgType]
argTypes (Definition (Pair (Const x) _)) = x

-- | @evalDefinition d i@ executes a definition on the input i. If the parser fails, it returns Nothing, otherwise
-- it returns Just the result of the execution.
evalDefinition :: (V.Unbox t) => Definition t a -> V.Vector t -> Maybe a
evalDefinition (Definition (Pair _ (ArgParser n f))) i
  | V.length i == n = Just $ f i
  | otherwise = Nothing

-- | @raw n@ just returns n tokens unprocessed.
raw :: Int -> Definition t (V.Vector t)
raw = arg Raw ?? id

-- | @address@ parses an address. It has a length of 2.
address :: Definition MemoryByte Address
address = arg MemoryAddress 2 $ \v -> fromIntegral (v V.! 0) .|. fromIntegral  (v V.! 1) `shiftL` 16

-- | @register df@ parses an register using df as the DataFlags. It has a length of 1.
register :: Dataflags -> Definition MemoryByte Register
register = error "TODO"
