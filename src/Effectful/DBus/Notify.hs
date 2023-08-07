-- |A library for issuing notifications using FreeDesktop.org Desktop
-- Notifications protocol. This protocol is used to communicate with services
-- such as Ubuntu's NotifyOSD.

module Effectful.DBus.Notify
  (
  -- * Usage
  -- $usage

  -- * Displaying notifications
    runNotify
  , NotifyError
  , withClient
  , notify
  , replace
  , Actions (..)
  , Notification
  , connectSession
  , Client
  , Notify
  -- * Manipulating action handlers
  , ActionHandlers
  , addActionHandlers
  , removeActionHandlers
  , uninstallActionsHandler
  -- * Constructing notifications
  , blankNote
  , Note (..)
  , Body (..)
  , URL
  , Timeout (..)
  , Action (..)
  , Icon (..)
  , Category (..)
  , UrgencyLevel (..)
  , Hint (..)
  -- * Capabilities
  , getCapabilities
  , Capability (..)
  ) where

import DBus
import DBus.Client hiding (throwError)

import Data.Maybe

import Effectful
import Effectful.Dispatch.Static
import Effectful.Error.Dynamic
import Effectful.Internal.Utils

import Effectful.DBus.Notify.Internal
import Effectful.DBus.Notify.Action

-- |A 'Note' with default values.
-- All fields are blank except for 'expiry', which is 'Dependent'.
blankNote :: Note
blankNote = Note
  { appName  = ""
  , appImage = Nothing
  , summary  = ""
  , body     = Nothing
  , actions  = Actions []
  , hints    = []
  , expiry   = Dependent
  }

-- |Display a notification.
-- Return a handle which can be used to replace the notification.
-- Updates the Action handlers map and repalces the old signal handler.
notify
  :: ( Notify :> es
    , Error NotifyError :> es )
  => Note -> Eff es Notification
notify note = replace (Notification { notificationId = 0 }) note

-- |Replace a notification.
-- Same as `notify`, except an existing notification is replaced.
replace
  :: ( Notify :> es
    , Error NotifyError :> es )
  => Notification -> Note -> Eff es Notification
replace (Notification nid) (Note {..}) = do
  Notify (client, handlers) <- getStaticRep
  notification <- do
    mr <- unsafeEff_ $
      callNotificationMethod client "Notify" args
    extractId mr
  unsafeEff_ $ removeActionHandlers handlers notification
  addActionHandlers notification actions
  return notification
  where
    extractId mr =
      case methodReturnBody mr of
        [] -> throwError NoId
        h:_ -> case fromVariant h of
          Nothing -> throwError IdFormat
          Just nid'-> return $ Notification nid'
    args =
      [ toVariant appName
      , toVariant nid
      , toVariant $ fromMaybe "" $ iconString <$> appImage
      , toVariant summary
      , toVariant $ fromMaybe "" $ flattenBody <$> body
      , toVariant $ actionsArray actions
      , toVariant $ hintsDict hints
      , toVariant $ timeoutInt expiry
      ]

-- |Determine the server's capabilities
getCapabilities :: Notify :> es => Eff es [Capability]
getCapabilities = do
  Notify (client, _) <- getStaticRep
  unsafeEff_ $ do
    mr <- callNotificationMethod client "GetCapabilities" []
    return $ map readCapability
      . fromJust . fromVariant
      . head . methodReturnBody $ mr

-- |Override currently used DBus `Client`.
withClient :: Notify :> es => Client -> Eff es a -> Eff es a
withClient client = localStaticRep (\(Notify (_, handlers)) -> Notify (client, handlers))

runNotify
  :: (IOE :> es, Error NotifyError :> es)
  => Client ->
  Eff (Notify : es) a
  -> Eff es (SignalHandler, a)
runNotify client m = do
  handlers <- liftIO $ newMVar' (ActionHandlers mempty)
  evalStaticRep (Notify (client, handlers)) $
    (,) <$> installActionsHandler <*> m
