{-# LANGUAGE DeriveDataTypeable #-}
module Haemu.Exception
 ( UnknownOpcode(..)
 , InvalidAddress(..)
 , InvalidRegister(..)
 , UnknownOptype(..)
 , (?->)
 ) where

import Haemu.Types
import Control.Monad.Exception

-- | Raised when an opcode is not a valid instruction.
-- The argument is the opcode that is unhandled.
data UnknownOpcode = UnknownOpcode Opcode deriving (Typeable, Show)
instance Exception UnknownOpcode

-- | Raised when the type of an instruction doesn't have any meaning assigned to it.
data UnknownOptype = UnknownOptype Optype deriving (Typeable, Show)
instance Exception UnknownOptype

-- | Raised when something is trying to access an invalid address.
data InvalidAddress = InvalidAddress Address deriving (Typeable, Show)
instance Exception InvalidAddress

-- | Raised when something is trying to access an invalid register.
data InvalidRegister = InvalidRegister Register deriving (Typeable, Show)
instance Exception InvalidRegister

-- | m ?-> e throws exception e when m is a Nothing, otherwise it returns the value in the Just.
(?->) :: (Monad m, Exception e, Throws e l) => Maybe a -> e -> EMT l m a
(?->) Nothing  = throw
(?->) (Just a) = const $ return a
infixl 1 ?->
