{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Haemu.Monad
  (
  -- * Types
    HaemuM
  , HaemuState(..)
  , Immutable
  , Mutable
  -- * Running Haemu actions
  , runHaemuM
  , execHaemuM
  , evalHaemuM
  , registers
  , memory
  , flags
  -- * Using the Haemu monad
  , iaccess
  , iwrite
  , access
  , write
  , htryuse
  , hperform
  -- * Implementation helpers
  , ImmOrMut
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.StateRef
import qualified Data.Vector.Unboxed         as V.I
import qualified Data.Vector.Unboxed.Mutable as V.M
import           Data.Word
import           Data.Word.Odd
import           Haemu.Types

-- | The indicator data type for immutable objects (for usage with 'ImmOrMut')
data Immutable

-- | The indicator data type for mutable objects in the monad m (for usage with 'ImmOrMut')
data Mutable (m :: * -> *)

-- | Gives either the mutable or the immutable version of a given immutable object o by switching on
-- an indicator datatype i.
type family ImmOrMut i o :: *
type instance ImmOrMut Immutable o = o
type instance ImmOrMut (Mutable m) (V.I.Vector a) = V.M.MVector (PrimState m) a
type instance ImmOrMut (Mutable m) Word16 = Ref m Word16
type instance ImmOrMut (Mutable m) Word8 = Ref m Word8
type instance ImmOrMut (Mutable m) Word32 = Ref m Word32
type instance ImmOrMut (Mutable m) Word64 = Ref m Word64
type instance ImmOrMut (Mutable m) (OddWord a n) = Ref m (OddWord a n)


-- | Holds the state for the Haemu monad. The type parameter i determines whether to use mutable or
-- immutable fields.
data HaemuState v = HaemuState
  { _flags     :: ImmOrMut v Flags                 -- ^ The state of the flag register
  , _registers :: ImmOrMut v (V.I.Vector Register) -- ^ The state of the registers
  , _memory    :: ImmOrMut v DataBlock             -- ^ The state of the memory
  }

-- Those instances are for testing
deriving instance (Show (ImmOrMut v DataBlock), Show (ImmOrMut v Flags)) => Show (HaemuState v)
deriving instance (Eq (ImmOrMut v DataBlock), Eq (ImmOrMut v Flags)) => Eq (HaemuState v)

-- Create lenses for our state data type
makeLenses ''HaemuState

-- | The haemu monad. The haemu monad manages a state of the registers and the memory. The type
-- argument is the state monad type m (IO or ST). There is also a call stack, stored in a state
-- monad.
type HaemuM m = StateT [Address] (ReaderT (HaemuState (Mutable m)) m)

-- | Convert an 'ImmutableHaemuState' to a 'MutableHaemuState'
thawState :: (Applicative f, PrimMonad f, HasRef f) => HaemuState Immutable -> f (HaemuState (Mutable f))
thawState (HaemuState f r m) = HaemuState <$> newRef f <*> V.I.thaw r <*> V.I.thaw m

-- | Unsafely convert a MutableHaemuState to an 'ImmutableHaemuState'. Make sure the mutable
-- state isn't ever used again after calling this function!
unsafeFreezeState :: (Applicative f, PrimMonad f) => HaemuState (Mutable f) -> f (HaemuState Immutable)
unsafeFreezeState (HaemuState f r m) = HaemuState <$> readRef f <*> V.I.unsafeFreeze r <*> V.I.unsafeFreeze m

-- | Given an initial state, run a Haemu computation in the underlying monad, returning the
-- resulting value and state.
runHaemuM :: (Applicative m, PrimMonad m, HasRef m) => HaemuM m a -> HaemuState Immutable -> m (a, HaemuState Immutable)
runHaemuM m s = do
  s' <- thawState s
  (,) <$> runReaderT (evalStateT m []) s' <*> unsafeFreezeState s'

-- | Same as 'runHaemuM', but this function only returns the final state and not the value of
-- the computation.
execHaemuM :: (Applicative m, PrimMonad m, HasRef m) => HaemuM m a -> HaemuState Immutable -> m (HaemuState Immutable)
execHaemuM m s = snd <$> runHaemuM m s

-- | Same as 'runHaemuM', but this function only returns the value of the computation and not the
-- final state.
evalHaemuM :: (Applicative m, PrimMonad m, HasRef m) => HaemuM m a -> HaemuState Immutable -> m a
evalHaemuM m s = fst <$> runHaemuM m s

-- We need seperate getters & setters because the guys from the lens package haven't discovered a
-- nice representation for monadic lenses yet. (There are only monadic folds & getters)

-- | @iaccess i@ is a 'MonadicFold' with the element at index @i@ in the vector if that element
-- exists and with no elements otherwise.
iaccess :: (PrimMonad m, V.M.Unbox a, Functor m) => Int -> MonadicFold m (V.M.MVector (PrimState m) a) a
iaccess i f v
  | i >= V.M.length v = pure v
  | otherwise = act (V.M.read ?? i) f v

-- | @iwrite i v@ is a 'Getter' returning an action that sets the value at index i to v in a given
-- vector.
iwrite :: (PrimMonad m, V.M.Unbox a) => Int -> a -> Action m (V.M.MVector (PrimState m) a) ()
iwrite i a = act $ \v -> V.M.write v i a

-- | @access@ is an 'Action' that, given a 'Ref', returns a monadic action with it's value.
access :: Action m (Ref m a) a
access = act readRef

-- | @write v@ is an 'Action' that, given a 'Ref', returns a monadic action that sets the value of
-- that 'Ref' to @v@.
write :: a -> Action m (Ref m a) ()
write a = act (writeRef ?? a)

-- | Like 'perform' from lens, with the difference that it takes the source value for the lens from
-- reader monad transformer layer around the monad the action runs in.
hperform :: (Monad (t' m), Monad m, MonadTrans t, MonadTrans t', MonadReader a (t (t' m))) => Acting m b a b -> t (t' m) b
hperform a = ask >>= lift . lift . perform a

-- | Returns the first element of a monadic fold using the reader as the input to the monadic fold.
-- If the fold is empty, it returns Nothing. This is to '(^!?)' like 'hperform' is to 'perform'
htryuse f = ask >>= lift . lift . (^!? f)
