{-# LANGUAGE FlexibleContexts #-}
module Instances where

import Test.QuickCheck
import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Data.Word.Odd
import Data.Bits

instance (Arbitrary a, V.Unbox a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = map V.fromList . shrink . V.toList

instance (TypeNum n, Bits a, Integral a) => Arbitrary (OddWord a n) where
  arbitrary = arbitraryBoundedIntegral
  shrink = pure . (`shiftR` 1)
