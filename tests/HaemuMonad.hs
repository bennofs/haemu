{-# LANGUAGE RankNTypes, TemplateHaskell, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction #-}
module HaemuMonad where

import Instances
import Haemu.Monad
import Control.Lens hiding (elements)
import Control.Applicative
import Test.QuickCheck
import Test.Hspec
import Data.Monoid
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import Data.Word

-- | A handy type alias
type IntState = ImmutableHaemuState Int Int

-- | Another one
type NEIntState = HaemuState (NonEmptyVector Int) (NonEmptyVector Int)

-- | QuickCheck modifier for generating non-empty vectors
newtype NonEmptyVector a = NonEmptyVector (V.Vector a) deriving (Show)
makeIso ''NonEmptyVector

-- | Type of a store. One can either store values in memory or in registers.
data Store = Register | Memory deriving (Show)

instance Arbitrary Store where
  arbitrary = elements [Register, Memory]

instance (Arbitrary a, V.Unbox a) => Arbitrary (NonEmptyVector a) where
  arbitrary = (\(NonEmpty l) -> V.fromList l ^. nonEmptyVector) <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (HaemuState a b) where
  arbitrary = HaemuState <$> arbitrary <*> arbitrary

-- | Convert a 'Store' to a getting for that store type.
storeToGetter :: Store -> Getter (HaemuState s s) s
storeToGetter Register = registers
storeToGetter Memory = memory

-- | Bind a function taking a store lens with 'Store' value.
withStore :: ((forall s. Getter (HaemuState s s) s) -> b) -> Store -> b
withStore f st = f (storeToGetter st)

-- | Extend a function taking a valid index ainto a function taking an arbitrary positive
-- integer.
withValidIndex :: (V.Unbox s) => HaemuState (NonEmptyVector s) (NonEmptyVector s)
               -> Getting (V.Vector s) (ImmutableHaemuState s s) (V.Vector s)
               -> (Int -> b)
               -> (Positive Int -> b)
withValidIndex st s f (Positive n) = f n'
  where l = V.length $ nonEmptyState st ^. s
        n' = n `rem` l

-- | Remove the NonEmptyVector wrapper from a 'HaemuState'.
nonEmptyState :: (V.Unbox a, V.Unbox b)
              => HaemuState (NonEmptyVector a) (NonEmptyVector b) -> ImmutableHaemuState a b
nonEmptyState s = s & registers %~ review nonEmptyVector & memory %~ review nonEmptyVector

-- | (==) combined with fmap
(==|) :: (Functor f, Eq a) => a -> f a -> f Bool
(==|) = fmap . (==)
infixr 3 ==|   -- (*>) is infixl 4

-- | 'defaultState' lifted to continuation-passing style (CPS) and with a NonEmptyVector wrapper.
withDefaultState :: (V.Unbox r, V.Unbox m)
                 => r -> m -> Positive (NonZero Word8) -> Positive (NonZero Word8)
                 -> (HaemuState (NonEmptyVector r) (NonEmptyVector m) -> b) -> b
withDefaultState r m (Positive (NonZero rc)) (Positive (NonZero mc)) f =
  f $ HaemuState
      (view nonEmptyVector $ V.replicate (fromEnum rc) r)
      (view nonEmptyVector $ V.replicate (fromEnum mc) m)

-- | Eval a HaemuM using a ST monad and the supplied initial state.
evalHaemuST :: (V.Unbox m, V.Unbox r)
            => ImmutableHaemuState r m -> (forall s. HaemuM (ST s) r m a) -> a
evalHaemuST s m = runST $ evalHaemuM m s

describeHaemuMonad :: Spec
describeHaemuMonad = describe "Haemu.Monad" $ do

  describe "runHaemuM" $ do

    it "returns the argument of the last 'return' as first tuple value" $
       property $ \(s :: IntState) (v :: Int) ->
       runST (fst <$> runHaemuM (return v) s) == v

    it "returns the initial state if no modifications are made as second tuple value" $
       property $ \(s :: IntState) ->
       runST (snd <$> runHaemuM (return ()) s) == s

  describe "execHaemuM" $ do

    it "returns the same value as the second element of runHaemuM's return value" $
       property $ \(s :: IntState) ->
       runST $ (==) . snd <$> runHaemuM (return ()) s <*> execHaemuM (return ()) s

  describe "evalHaemuM" $ do

    it "returns the same value as the first element of runHaemuM's return value" $
       property $ \(s :: IntState) (v :: Int) ->
       runST $ (==) . fst <$> runHaemuM (return v) s <*> evalHaemuM (return v) s

  describe "write" $ do

    it "overwrites previous writes" $
       property                       $ \s (v1 :: Int) (v2 :: Int) ->
       withStore                      $ \st ->
       withValidIndex s st            $ \i ->
       evalHaemuST (nonEmptyState s)  $
       v2 ==| hperform (st . write i v1) *> hperform (st . write i v2) *> hperform (st . access i)

    it "only changes the given position, for all other positions the value stays the same" $
       property                                   $ \s (v1 :: Int) (v2 :: Int) ->
       withStore                                  $ \st  ->
       withValidIndex s st                        $ \i1 ->
       withValidIndex s st                        $ \i2 ->
       i1 /= i2 ==> evalHaemuST (nonEmptyState s)  $
       v2 ==| hperform (st . write i1 v2) *> hperform (st . write i2 v1) *> hperform (st . access i1)

  describe "access" $ do

    it "returns the initial state if no write to that position happened already" $
       property                       $ \(r :: Int) (v :: Int) rc vc ->
       withDefaultState r v rc vc     $ \sn ->
       withStore                      $ \s  ->
       withValidIndex sn s            $ \i  ->
       evalHaemuST (nonEmptyState sn) $
       V.head (nonEmptyState sn ^. s) ==| hperform (s . access i)

    it "returns the value of the last write if there exists one" $
      property                       $ \(sn :: NEIntState) (v :: Int) ->
      withStore                      $ \s ->
      withValidIndex sn s            $ \i ->
      evalHaemuST (nonEmptyState sn) $
      v ==| hperform (s . write i v) *> hperform (s . access i)
