module Effectful.DBus.Notify.Action where

import Data.Map.Strict qualified as M
import Data.Word
import Data.Time

newtype ActionHandlers = ActionHandlers (M.Map (Word32, String) (IO (), Maybe UTCTime))