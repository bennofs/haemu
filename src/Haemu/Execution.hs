{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Haemu.Execution where

import Haemu.Monad
import Haemu.Unit
import Haemu.Instruction
import Haemu.Exception
import Control.Lens
import Data.Monoid
import Control.Monad.Exception
import qualified Data.Vector as V

data ExecConfig l m r v = ExecConfig
  { _units :: V.Vector (Unit l m r v)
  }
makeLenses ''ExecConfig

-- | We can combine multiple configurations into one single configuration.
instance Monoid (ExecConfig l m r v) where
  mempty = ExecConfig V.empty
  mappend a = units <>~ a ^. units -- ^ This gives the second operand preference.

executeInstruction :: (Monad m, Throws UnknownOptype l)
                   => ExecConfig l m r v -> Instruction -> EMT l (HaemuM m r v) ()
executeInstruction c i = do
  u <- c ^? units . ix (i ^. optype . int) ?-> UnknownOptype (i ^. optype)
  runInstruction u i
