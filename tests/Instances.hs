module Instances where

import Test.QuickCheck
import qualified Data.Vector.Unboxed as V
import Control.Applicative

instance (Arbitrary a, V.Unbox a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = map V.fromList . shrink . V.toList
