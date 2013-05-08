{-# LANGUAGE RankNTypes #-}
module LensLaws where

import Control.Lens
import Test.Hspec
import Test.QuickCheck

-- | Checks the laws that have to hold for a lens
describeLens :: (Eq a, Eq s, Show a, Show s, Arbitrary s, Arbitrary a) => Lens' s a -> Spec
describeLens l = describe "follows lens laws" $ do

  it "gets back what you put in" $
     property $ \s a -> view l (set l a s) == a

  it "doesn't change anything if you put back the value you got" $
     property $ \a -> set l (view l a) a == a

  it "is the same when you set twice or when you only set the second value" $
     property $ \s a b -> set l b (set l a s) == set l b s

-- | Checks the laws that have to hold for an isomorphism
describeIso :: (Eq a, Eq b, Show a, Show b, Arbitrary a, Arbitrary b) => Iso' a b -> Spec
describeIso i = describe "follows the isomorphism laws" $ do

  it "is the same as identity when composed with the inverse" $ do
    property $ \a -> review i (view i a) == a

  it "is the same as identity when the inverse is composed with it" $ do
    property $ \b -> view i (review i b) == b

-- | Checks the laws that have to hold for a prism.
describePrism :: (Eq s, Eq a, Show a, Show s, Arbitrary a, Arbitrary s) => Prism' s a -> Spec
describePrism p = describe "follows the prism laws" $ do

  it "gets back a value reviewed through it in a Just" $
    property $ \a -> (p # a) ^? p == Just a

  it "value s is completely described by the value I got when viewing (when possible) and the prism" $
    property $ \s -> maybe True (\a -> (p # a) == s) $ s ^? p
