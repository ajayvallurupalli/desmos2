module Capability.Log where

import Prelude

import Data.DateTime (DateTime)
import Halogen (HalogenM, lift)

data Log 
  = Error DateTime String 
  | Debug String

class Monad m <= LogMessage m where
  log :: Log -> m Unit
  dumpState :: Unit -> m Unit

instance logMessageHalogenM :: LogMessage m => LogMessage (HalogenM st act slots msg m) where
  log = lift <<< log 
  dumpState = lift <<< dumpState
