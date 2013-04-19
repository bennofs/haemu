{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections, NoMonomorphismRestriction, TemplateHaskell         #-}
import           Control.Applicative
import           Control.Lens
import           Control.Monad.ST
import           Data.Array.Unboxed
import           Data.List
import           Haemu.Monad
import           Test.Hspec
import           Test.QuickCheck
import Test.QuickCheck.Modifiers
import Control.Monad
import Debug.Trace
import Control.Exception (evaluate)

-- | A handy alias, as this is the type used most of the time for our quick checks.
type IntState a = HaemuState (a Int Int) (a Int Int)

-- | Newtype wrapper so we can have an 'Arbitrary' instance for generating non-empty arrays.
newtype NonEmptyArray a i e = NonEmptyArray (a i e) deriving (Show)
makeIso ''NonEmptyArray

-- | Make an array from a list of elements, starting with the index 0.
arrFromList :: (IArray a e, Ix i, Num i) => [e] -> a i e
arrFromList = flip listArray <*> (0,) . genericLength

-- | Convert an array to a list of elements, such that arrFromList . arrToList = id
arrToList :: (IArray a e, Ix i) => a i e -> [e]
arrToList = elems

-- | Turn a function expecting a valid register id / memory address in a given state in a function
-- allowing any value to be passed to it.

withValid :: Getting (NonEmptyArray UArray Int Int) (IntState (NonEmptyArray UArray)) (NonEmptyArray UArray Int Int)
          -> IntState (NonEmptyArray UArray)
          -> (Int -> b)
          -> Int -> b
withValid l s f n = f . mod n . snd . bounds . review nonEmptyArray . view l $ s

instance (IArray a e, Num i, Ix i, Arbitrary i, Arbitrary e) => Arbitrary (a i e) where
  arbitrary = arrFromList <$> arbitrary

instance (Arbitrary (c Int r), Arbitrary (d a v)) => Arbitrary (HaemuState (c Int r) (d a v)) where
  arbitrary = HaemuState <$> arbitrary <*> arbitrary

instance (IArray a e, Num i, Ix i, Arbitrary i, Arbitrary e) => Arbitrary (NonEmptyArray a i e) where
  arbitrary = (\(NonEmpty l) -> NonEmptyArray $ arrFromList l) <$> arbitrary

-- | Convert an IntState NonEmptyArray to an IntState UArray
nonEmptyState :: IntState (NonEmptyArray UArray) -> IntState UArray
nonEmptyState s = s & registers %~ review nonEmptyArray
                    & memory    %~ review nonEmptyArray

-- | @initState nreg nmem@ creates a new IntState with nreg registers and nmen values
-- of memory, all filled with 0. The first register / memory value has index 0, the last
-- has index @pred nreg@/@pred nmen@.
initState :: Int -> Int -> IntState UArray
initState nreg nmen = HaemuState (nullArr nreg) $ nullArr nmen
  where nullArr x = listArray (0, pred x) $ repeat x

main :: IO ()
main = hspec $ do
  describe "Haemu.Monad.runHaemuM" $ do

    it "returns the value of the last 'return' as first element of the tuple" $
      property $ \(s :: IntState UArray) (v :: Int) -> fst (runST (runHaemuM ?? s $ return v)) == v

    it "returns the last state as second element of the tuple" $
      property $ \(s :: IntState UArray) -> snd (runST (runHaemuM ?? s $ return ())) == s

  describe "Haemu.Monad.readRegister" $ do

    it "doesn't change the state" $ do
      property $ \s -> let s' = nonEmptyState s in withValid registers s $
        \r -> (s' ==) $ snd $ runST $ runHaemuM (readRegister r) s'

    it "reads the initial state if no previous write occured" $
      property $ \s -> let s' = nonEmptyState s in withValid registers s $ \r ->
        (== s' ^? registers . ix r) $ preview (_2 . registers . ix r) $ runST $
           runHaemuM (readRegister r) s'

    it "throws an exception when register id is out of range" $
      void $ evaluate (runST $ runHaemuM (readRegister 100) $ initState 1 1) `shouldThrow` anyException

  describe "Haemu.Monad.writeRegister" $ do

    it "sets the value of the register, so the next read from that position will return the set value" $
      property $ \v s -> withValid registers s $
        \r -> (v ==) $ fst $ runST $ runHaemuM ?? (nonEmptyState s) $
          writeRegister r v >> readRegister r

    it "overwrites any previous writes" $
      property $ \v1 v2 s -> withValid registers s $
        \r -> (v2 ==) $ fst $ runST $ runHaemuM ?? (nonEmptyState s) $
          writeRegister r v1 >> writeRegister r v2 >> readRegister r

    it "doesn't affect other registers" $
      property $ \s v1 v2 -> withValid registers s $ \r1 -> withValid registers s $ \r2 ->
        (r1 /= r2 ==>) $ (== v1) $ fst $ runST $ runHaemuM ?? (nonEmptyState s) $ do
          writeRegister r1 v1
          writeRegister r2 v2
          readRegister r1

    it "should throw when used with a non-existent register" $
      void $ evaluate (runST $ runHaemuM (writeRegister 100 10) $ initState 3 0) `shouldThrow` anyException
