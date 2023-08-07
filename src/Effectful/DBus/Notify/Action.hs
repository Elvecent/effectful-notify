module Effectful.DBus.Notify.Action where

import DBus
import DBus.Client hiding (throwError)

import GHC.Generics
import Data.Word
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Time

import Effectful
import Effectful.Dispatch.Static
import Effectful.Internal.Utils

import Effectful.DBus.Notify.Internal

newtype ActionHandlers = ActionHandlers (M.Map (Word32, String) (IO ()))
  deriving Generic

insertActionHandler
  :: Notification
  -> String
  -> IO ()
  -> ActionHandlers
  -> ActionHandlers
insertActionHandler (Notification nid) nkey handler (ActionHandlers m) =
  ActionHandlers $ M.insert (nid, nkey) handler m

lookupActionHandler :: Notification -> String -> ActionHandlers -> Maybe (IO ())
lookupActionHandler (Notification nid) nkey (ActionHandlers m) =
  M.lookup (nid, nkey) m

filterActionHandlers
  :: ((Notification, String) -> Bool)
  -> ActionHandlers -> ActionHandlers
filterActionHandlers predicate (ActionHandlers m)
  = ActionHandlers $ M.filterWithKey
    (\(nid, nkey) _ -> predicate (Notification nid, nkey)) m

-- |Attach handlers for given `Actions` to the notification.
-- This merely updates the state, the actual handler is set by `installActionsHandler` in `runNotify`.
addActionHandlers
  :: Notify :> es
  => Notification -> Actions -> Eff es ()
addActionHandlers notification (Actions actions) = do
  Notify (_, handlers) <- getStaticRep
  unsafeEff_ $ traverse_ (addHandler handlers) actions
  where
    addHandler handlers (Action {..}) = modifyMVar' handlers $ \handlerMap ->
      return ( insertActionHandler notification actionName actionHandler handlerMap
             , () )

-- |Detach all handlers from the notification.
-- This merely updates the state, the actual handler is removed by `uninstallActionsHandler`.
removeActionHandlers
  :: MVar' ActionHandlers -> Notification -> IO ()
removeActionHandlers handlers notification =
  modifyMVar' handlers $ \handlerMap ->
  return (filterActionHandlers notStale handlerMap, ())
  where
    notStale (notification', handler) =
      notification /= notification' && notExpired handler
    notExpired _ = True

-- |Make the session listen to DBus Notifications.
-- The handler dispatches signals using the current state.
installActionsHandler
  :: Notify :> es
  => Eff es SignalHandler
installActionsHandler = do
  Notify (client, ahm) <- getStaticRep
  unsafeEff_ $
    addMatch client matchRule (mkHandler ahm)
  where
    mkHandler ahm sig = do
      handlerMap <- readMVar' ahm
      case handlerMap of
        ActionHandlers handlers -> print $ M.keys handlers
      let
        actHandler = do
          (nid, nkey) <- getActionKey sig
          (nid,) <$> lookupActionHandler nid nkey handlerMap
      case actHandler of
        Nothing -> pure ()
        Just (nid, h) -> do
          removeActionHandlers ahm nid
          h
    matchRule = matchAny
      { matchInterface = Just nInterfaceName
      , matchPath      = Just nObjectPath
      , matchMember    = Just "ActionInvoked"
      }      

-- |Stop listening to DBus Notifications.
uninstallActionsHandler :: Client -> SignalHandler -> IO ()
uninstallActionsHandler = removeMatch

getActionKey :: Signal -> Maybe (Notification, String)
getActionKey sig =
  readKey $ signalBody sig
  where
    readKey (aid:aname:[]) = do
      actId <- fromVariant @Word32 aid
      actName <- fromVariant @String aname
      return (Notification actId, actName)
    readKey _ = Nothing
