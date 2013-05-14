{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Haemu.Execution where

import Haemu.Monad
import Haemu.Unit
import Haemu.Instruction
import Haemu.Exception
import Haemu.Types
import Control.Lens
import Data.Monoid
import Control.Monad.Exception
import qualified Data.Vector as V

data ExecConfig l m = ExecConfig
  { _units :: V.Vector (Unit l m)
  }
makeLenses ''ExecConfig

-- | We can combine multiple configurations into one single configuration.
instance Monoid (ExecConfig l m) where
  mempty = ExecConfig V.empty
  mappend a = units <>~ a ^. units -- ^ This gives the second operand preference.

executeInstruction :: (Monad m, Throws UnknownOptype l) 
                       => ExecConfig l m -> HaemuState v -> Instruction -> EMT l (HaemuM m) ()
executeInstruction c s i = do
  if checkCondition (i ^. condition) (s ^. flags)
    then do
      u <- c ^? units . ix (i ^. optype . int) ?-> UnknownOptype (i ^. optype)
      runInstruction u i
	else -- TODO

checkCondition :: Condition -> Flags -> Boolean
checkCondition 0 _ = True
--TODO
checkCondition 256 _ = False
