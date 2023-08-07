module Effectful.DBus.Notify.Action where

import           DBus
import           DBus.Client                    hiding (throwError)

import           Data.Foldable
import qualified Data.Map.Strict                as M
import           Data.Time
import           Data.Word
import           GHC.Generics

import           Effectful
import           Effectful.Dispatch.Static
import           Effectful.Internal.Utils

import           Effectful.DBus.Notify.Internal

newtype ActionHandlers =
  ActionHandlers (M.Map (Word32, String) (IO (), Maybe UTCTime))
  deriving Generic

insertActionHandler
  :: Notification
  -> String
  -> IO ()
  -> Maybe UTCTime
  -> ActionHandlers
  -> ActionHandlers
insertActionHandler
  (Notification nid) nkey handler expiration (ActionHandlers m) =
  ActionHandlers $ M.insert (nid, nkey) (handler, expiration) m

lookupActionHandler
  :: Notification -> String -> ActionHandlers -> Maybe (IO (), Maybe UTCTime)
lookupActionHandler (Notification nid) nkey (ActionHandlers m) =
  M.lookup (nid, nkey) m

filterActionHandlers
  :: ((Notification, String) -> Maybe UTCTime -> Bool)
  -> ActionHandlers -> ActionHandlers
filterActionHandlers predicate (ActionHandlers m)
  = ActionHandlers $ M.filterWithKey
    (\(nid, nkey) (_, expiration) ->
       predicate (Notification nid, nkey) expiration) m

-- |Attach handlers for given `Actions` to the notification.
-- This merely updates the state, the actual handler is set by `installActionsHandler` in `runNotify`.
addActionHandlers
  :: Notify :> es
  => Notification -> Maybe UTCTime -> Actions -> Eff es ()
addActionHandlers notification expiration (Actions actions) = do
  Notify (_, handlers) <- getStaticRep
  unsafeEff_ $ traverse_ (addHandler handlers) actions
  where
    addHandler handlers (Action {..}) = modifyMVar' handlers $ \handlerMap ->
      return ( insertActionHandler
                 notification
                 actionName
                 actionHandler
                 expiration
                 handlerMap
             , () )

-- |Detach all handlers from the notification.
-- This merely updates the state, the actual handler is removed by `uninstallActionsHandler`.
removeActionHandlers
  :: MVar' ActionHandlers -> Notification -> IO ()
removeActionHandlers handlers notification = do
  ctime <- getCurrentTime
  modifyMVar' handlers $ \handlerMap ->
    return (filterActionHandlers (notStale ctime) handlerMap, ())
  where
    notStale ctime (notification', _) expiration =
      notification /= notification' && notExpired
      where
        notExpired = case expiration of
          Just e  -> ctime < e
          Nothing -> True

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
      let
        actHandler = do
          (nid, nkey) <- getActionKey sig
          (nid,) <$> lookupActionHandler nid nkey handlerMap
      case actHandler of
        Nothing -> pure ()
        Just (nid, (h,_)) -> do
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
