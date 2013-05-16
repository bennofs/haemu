{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module HaemuMonad where

import           Control.Applicative
import           Control.Lens        hiding (elements)
import           Control.Monad.ST
import           Data.Monoid
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Haemu.Monad
import           Haemu.Types
import           Instances
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary (HaemuState Immutable) where
  arbitrary = HaemuState <$> arbitrary <*> arbitrary <*> arbitrary

data Memory = Memory
data Registers = Registers

class CloneStore s a | s -> a where
  cloneStore :: s -> Getter (HaemuState i) (ImmOrMut i (V.Vector a))

instance CloneStore Memory MemoryByte where cloneStore _ = memory
instance CloneStore Registers Register where cloneStore _ = registers

-- | Call a function once using registers and second using memory, combining the results with (.&&.) from QuickCheck.
withStore :: (forall s. forall a. (CloneStore s a, Arbitrary a, Show a, Eq a, V.Unbox a) => s -> Property) -> Property
withStore f = f Memory .&&. f Registers

-- | Extend a function taking a valid index into a function taking an arbitrary positive
-- integer.
withValidIndex :: (V.Unbox s, Testable b) => HaemuState Immutable -> Getting (V.Vector s) (HaemuState Immutable) (V.Vector s) -> (Int -> b) -> NonNegative Int -> Property
withValidIndex st s f (NonNegative n) = V.length (st ^. s) /= 0 ==> f n'
  where l = V.length $ st ^. s
        n' = n `rem` l

-- | (==) combined with fmap
(==|) :: (Functor f, Eq a) => a -> f a -> f Bool
(==|) = fmap . (==)
infixr 3 ==|   -- (*>) is infixl 4

-- | 'defaultState' lifted to continuation-passing style (CPS).
withDefaultState :: Register -> MemoryByte -> Positive Word8 -> Positive Word8
                 -> (HaemuState Immutable -> b) -> b
withDefaultState r m (Positive rc) (Positive mc) f =
  f $ HaemuState 0 (V.replicate (fromEnum rc) r) (V.replicate (fromEnum mc) m)

-- | Eval a HaemuM using a ST monad and the supplied initial state.
evalHaemuST :: HaemuState Immutable -> (forall s. HaemuM (ST s) a) -> a
evalHaemuST s m = runST $ evalHaemuM m s

describeHaemuMonad :: Spec
describeHaemuMonad = describe "Haemu.Monad" $ do

  describe "runHaemuM" $ do

    it "returns the argument of the last 'return' as first tuple value" $
       property $ \s (v :: Int) ->
       runST (fst <$> runHaemuM (return v) s) == v

    it "returns the initial state if no modifications are made as second tuple value" $
       property $ \s ->
       runST (snd <$> runHaemuM (return ()) s) == s

  describe "execHaemuM" $

    it "returns the same value as the second element of runHaemuM's return value" $
       property $ \s ->
       runST $ (==) . snd <$> runHaemuM (return ()) s <*> execHaemuM (return ()) s

  describe "evalHaemuM" $

    it "returns the same value as the first element of runHaemuM's return value" $
       property $ \s (v :: Int) ->
       runST $ (==) . fst <$> runHaemuM (return v) s <*> evalHaemuM (return v) s

  describe "iwrite" $ do

    it "overwrites previous writes"       $
       property                           $ \s  ->
       withStore                          $ \st ->
       property                           $ \v1 v2 ->
       withValidIndex s (cloneStore st)   $ \i  ->
       evalHaemuST s                      $
       ($ (cloneStore st, cloneStore st)) $ \(st1, st2) ->
       Just v2 ==| hperform (st1 . iwrite i v1) *> hperform (st1 . iwrite i v2) *> htryuse (st2 . iaccess i)

    it "only changes the given position, for all other positions the value stays the same" $
       property                           $ \s  ->
       withStore                          $ \st ->
       property                           $ \v1 v2 ->
       withValidIndex s (cloneStore st)   $ \i1 ->
       withValidIndex s (cloneStore st)   $ \i2 ->
       i1 /= i2 ==> evalHaemuST s          $
       ($ (cloneStore st, cloneStore st)) $ \(st1, st2) ->
       Just v2 ==| hperform (st1 . iwrite i1 v2) *> hperform (st1 . iwrite i2 v1) *> htryuse (st2 . iaccess i1)

  describe "iaccess" $ do

    it "returns the initial state if no write to that position happened already" $
       withStore                          $ \s  ->
       property                           $ \r v rc vc ->
       withDefaultState r v rc vc         $ \sn ->
       withValidIndex sn (cloneStore s)   $ \i  ->
       evalHaemuST sn                     $
       (Just $ V.head $ sn ^. cloneStore s) ==| htryuse (cloneStore s . iaccess i)

    it "returns the value of the last iwrite if there exists one" $
      property                            $ \sn ->
      withStore                           $ \s  ->
      property                            $ \v  ->
      withValidIndex sn (cloneStore s)    $ \i  ->
      evalHaemuST sn                      $
      Just v ==| hperform (cloneStore s . iwrite i v) *> htryuse (cloneStore s . iaccess i)
