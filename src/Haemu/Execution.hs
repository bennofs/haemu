{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Haemu.Execution where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Exception
import           Data.Monoid
import qualified Data.Vector             as V
import           Haemu.Exception
import           Haemu.Instruction
import           Haemu.Monad
import           Haemu.Types
import           Haemu.Unit

data ExecConfig l m = ExecConfig
  { _units :: V.Vector (Unit l m)
  }
makeLenses ''ExecConfig

-- | We can combine multiple configurations into one single configuration.
instance Monoid (ExecConfig l m) where
  mempty = ExecConfig V.empty
  mappend a = units <>~ a ^. units -- ^ This gives the second operand preference.

executeInstruction :: (Monad m, Throws UnknownOptype l) => ExecConfig l m -> HaemuState Immutable -> Instruction -> EMT l (HaemuM m) ()
executeInstruction c s i = do
  when (checkCondition (i ^. condition) $ s ^. flags) $ do
      u <- c ^? units . ix (i ^. optype . int) ?-> UnknownOptype (i ^. optype)
      runInstruction u (i ^. opcode) (i ^. dataflags) (i ^. dataBlock)

checkCondition :: Condition -> Flags -> Bool
checkCondition 0 _ = True
-- TODO
checkCondition 256 _ = False
