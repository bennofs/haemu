{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module HaemuInstruction where

import LensLaws
import Instances
import Control.Lens

import Test.Hspec
import Test.QuickCheck
import Haemu.Instruction
import Data.Bits
import Control.Applicative
import Data.Word
import qualified Data.Vector.Unboxed as V

deriving instance Ord Instruction

instance Arbitrary Instruction where
  arbitrary = Instruction <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary `suchThat` ((< 16) . V.length)
  shrink i = filter (/= i) $
    [ i & opcode %~ lower
    , i & optype %~ lower
    , i & condition %~ lower
    , i & dataflags %~ lower
    ]
    where lower :: (Bits a) => a -> a
          lower = (`shiftR` 1)

describeInstructionModule :: Spec
describeInstructionModule = do
  describe "opcode is a lens" $ describeLens opcode
  describe "optype is a lens" $ describeLens optype
  describe "condition is a lens" $ describeLens condition
  describe "dataflags is a lens" $ describeLens dataflags
  describe "instruction is a prism" $ describePrism instruction
