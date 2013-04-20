{-# LANGUAGE RankNTypes, TemplateHaskell, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}
module HaemuMonad where

import Haemu.Monad
import Control.Lens hiding (elements)
import Control.Applicative
import Test.QuickCheck
import Test.Hspec
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V

-- | A handy type alias
type IntState = ImmutableHaemuState Int Int

-- | QuickCheck modifier for generating non-empty vectors
newtype NonEmptyVector a = NonEmptyVector (V.Vector a) deriving (Show)
makeIso ''NonEmptyVector

-- | Another one
type NEIntState = HaemuState (NonEmptyVector Int) (NonEmptyVector Int)

-- | Type of a store. One can either store values in memory or in registers.
data Store = Register | Memory deriving (Show)

-- | Convert a 'Store' to a getting for that store type.
storeToGetter :: Store -> (forall s. Getting s (HaemuState s s) s)
storeToGetter Register = registers
storeToGetter Memory = memory

-- | Extend a function taking a valid index ainto a function taking an arbitrary positive
-- integer.
withValidIndex :: (V.Unbox s) => HaemuState (NonEmptyVector s) (NonEmptyVector s)
               -> Store
               -> (Int -> b)
               -> (Positive Int -> b)
withValidIndex st s f (Positive n) = f n'
  where l = V.length $ nonEmptyState st ^. storeToGetter s
        n' = n `rem` l

instance Arbitrary Store where
  arbitrary = elements [Register, Memory]

instance (Arbitrary a, V.Unbox a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary a, V.Unbox a) => Arbitrary (NonEmptyVector a) where
  arbitrary = (\(NonEmpty l) -> V.fromList l ^. nonEmptyVector) <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (HaemuState a b) where
  arbitrary = HaemuState <$> arbitrary <*> arbitrary

nonEmptyState :: (V.Unbox a, V.Unbox b)
              => HaemuState (NonEmptyVector a) (NonEmptyVector b) -> ImmutableHaemuState a b
nonEmptyState s = s & registers %~ review nonEmptyVector & memory %~ review nonEmptyVector

describeHaemuMonad = describe "Haemu.Monad" $ do

  describe "runHaemuM" $ do

    it "returns the argument of the last 'return' as first tuple value" $
       property $ \(s :: IntState) (v :: Int) -> fst (runST $ runHaemuM (return v) s) == v

    it "returns the initial state if no modifications are made as second tuple value" $
       property $ \(s :: IntState) -> s == snd (runST $ runHaemuM (return ()) s)

  describe "execHaemuM" $ do

    it "returns the same value as the second element of runHaemuM's return value" $
       property $ \(s :: IntState) -> runST $ (==) . snd <$> runHaemuM (return ()) s <*> execHaemuM (return ()) s

  describe "evalHaemuM" $ do

    it "returns the same value as the first element of runHaemuM's return value" $
       property $ \(s :: IntState) (v :: Int) -> runST $ (==) . fst <$> runHaemuM (return v) s <*> evalHaemuM (return v) s

  describe "write" $ do

    it "overwrites previous writes" $
       property $ \(sn :: NEIntState) st (v1 :: Int) (v2 :: Int) -> let s = nonEmptyState sn in withValidIndex sn st $
         \i -> (== v2) $ runST $ evalHaemuM ?? (nonEmptyState sn) $ do
           write (storeToGetter st) i v1
           write (storeToGetter st) i v2
           access (storeToGetter st) i
