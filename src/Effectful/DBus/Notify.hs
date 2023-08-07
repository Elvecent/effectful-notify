{-# LANGUAGE
  TemplateHaskell,
  OverloadedStrings,
  OverloadedLabels,
  TypeFamilies,
  DataKinds,
  LambdaCase,
  DerivingStrategies,
  RecordWildCards
#-}

-- |A library for issuing notifications using FreeDesktop.org Desktop
-- Notifications protocol. This protocol is used to communicate with services
-- such as Ubuntu's NotifyOSD.

module Effectful.DBus.Notify
  (
  -- * Usage
  -- $usage

  -- * Displaying notifications
    runNotify
  , runEff
  , withClient
  , notify
  , replace
  , Actions (..)
  , Notification
  , connectSession
  , Client
  , Eff
  , (:>)
  , Notify
  , State
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

import GHC.Generics
import Data.Word
import Data.Foldable
import Data.Int
import Data.Maybe
import Data.Map.Strict qualified as M
import Data.Char (isLower, toLower)
import Control.Arrow (second, (&&&))
import Unsafe.Coerce
import Control.Concurrent.MVar

import Effectful
import Effectful.Dispatch.Static
import Effectful.State.Static.Shared
import Effectful.Error.Dynamic

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

-- |Action definition with a handler
data Action = Action
  { actionName      :: String
  , actionLabel     :: String
  , actionHandler   :: IO ()
  } deriving Generic

instance Show Action where
  show Action {..} = fold
    ["Action { actionName = "
    , show actionName
    , ", actionLabel = "
    , show actionLabel
    , ", actionHandler = _ }"
    ]

instance Eq Action where
  act1 == act2 =
       actionName act1 == actionName act2
    && actionLabel act1 == actionLabel act2

newtype Actions = Actions [Action]
  deriving stock (Generic)
  deriving newtype (Eq, Show)

-- |Contents of a notification
data Note = Note
  { appName :: String
  , appImage :: Maybe Icon
  , summary :: String
  , body :: Maybe Body
  , actions :: Actions
  , hints :: [Hint]
  , expiry :: Timeout
  } deriving (Eq, Show, Generic)

-- |Message bodies may contain simple markup.
-- NotifyOSD doesn't support any markup.
data Body
  = Text String
  | Bold Body
  | Italic Body
  | Underline Body
  | Hyperlink URL Body
  | Img URL String
  | Concat Body Body
  deriving (Eq, Show, Generic)

type URL = String

-- |Length of time to display notifications. NotifyOSD seems to ignore these.
data Timeout
  = Never              -- ^Wait to be dismissed by user
  | Dependent          -- ^Let the notification service decide
  | Milliseconds Int32 -- ^Show notification for a fixed duration
                       -- (must be positive)
  deriving (Eq, Show, Generic)

-- |An Icon is either a path to an image, or a name in an icon theme
data Icon = File FilePath | Icon String
  deriving (Eq, Show, Generic)

iconString :: Icon -> String
iconString (File fp) = "file://" ++ fp
iconString (Icon name) = name

-- |Urgency of the notification. Notifications may be prioritised by urgency.
data UrgencyLevel
  = Low
  | Normal
  | Critical -- ^Critical notifications require user attention
  deriving (Eq, Ord, Enum, Show, Generic)

-- |Various hints about how the notification should be displayed
data Hint
  = Urgency UrgencyLevel
  | Category Category
  | ImagePath Icon
  | SoundFile FilePath
  | SuppressSound Bool
  | X Int32
  | Y Int32
  deriving (Eq, Show, Generic)

-- |Categorisation of (some) notifications
data Category
  = Device | DeviceAdded | DeviceError | DeviceRemoved
  | Email | EmailArrived | EmailBounced
  | Im | ImError | ImReceived
  | Network | NetworkConnected | NetworkDisconnected | NetworkError
  | Presence | PresenceOffline | PresenceOnline
  | Transfer | TransferComplete | TransferError
  deriving (Eq, Show, Generic)

data NotifyError
  = NoId
  | IdFormat
  deriving (Eq, Generic)

instance Show NotifyError where
  show = \case
    NoId -> "Notification not found in D-Bus response"
    IdFormat -> "Could not read notification id from D-Bus response"

-- |A handle on a displayed notification
-- The notification may not have reached the screen yet, and may already have
-- been closed.
newtype Notification = Notification { notificationId :: Word32 }
  deriving (Eq, Show, Generic)

data Notify :: Effect

type instance DispatchOf Notify = Static WithSideEffects

newtype instance StaticRep Notify = Notify Client

-- |Display a notification.
-- Return a handle which can be used to replace the notification.
-- Updates the Action handlers map and repalces the old signal handler.
notify
  :: ( Notify :> es
    , State ActionHandlers :> es
    , Error NotifyError :> es )
  => Note -> Eff es Notification
notify note = replace (Notification { notificationId = 0 }) note

-- |Replace a notification.
-- Same as `notify`, except an existing notification is replaced.
replace
  :: ( Notify :> es
    , State ActionHandlers :> es
    , Error NotifyError :> es )
  => Notification -> Note -> Eff es Notification
replace (Notification nid) (Note {..}) = do
  Notify client <- getStaticRep
  notification <- do
    mr <- unsafeEff_ $
      callNotificationMethod client "Notify" args
    extractId mr
  removeActionHandlers notification
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
  Notify client <- getStaticRep
  unsafeEff_ $ do
    mr <- callNotificationMethod client "GetCapabilities" []
    return $ map readCapability
      . fromJust . fromVariant
      . head . methodReturnBody $ mr

-- |Override currently used DBus `Client`.
withClient :: Notify :> es => Client -> Eff es a -> Eff es a
withClient client = localStaticRep (const $ Notify client)

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
  :: ( Notify :> es
    , State ActionHandlers :> es )
  => Notification -> Actions -> Eff es ()
addActionHandlers notification (Actions actions) =
  traverse_ addHandler actions
  where
    addHandler (Action {..}) = modify $
      insertActionHandler notification actionName actionHandler

-- |Detach all handlers from the notification.
-- This merely updates the state, the actual handler is removed by `uninstallActionsHandler`.
removeActionHandlers
  :: ( State ActionHandlers :> es )
  => Notification -> Eff es ()
removeActionHandlers notification =
  modify $ filterActionHandlers notThisOne
  where
    notThisOne (notification', _) = notification /= notification'

-- |Make the session listen to DBus Notifications.
-- The handler dispatches signals using the current state.
installActionsHandler
  :: ( Notify :> es
    , State ActionHandlers :> es )
  => Eff es SignalHandler
installActionsHandler = do
  Notify client <- getStaticRep
  ahm <- unsafeCoerce <$> getStaticRep @(State ActionHandlers)
  unsafeEff_ $
    addMatch client matchRule (mkHandler ahm)
  where
    mkHandler ahm sig = do
      handlerMap <- readMVar ahm
      let
        actHandler = do
          (nid, nkey) <- getActionKey sig
          (nid,) <$> lookupActionHandler nid nkey handlerMap
      case actHandler of
        Nothing -> pure ()
        Just (nid, h) -> do
          runEff $ evalStateMVar ahm $ removeActionHandlers nid
          h
    matchRule = matchAny
      { matchInterface = Just nInterfaceName
      , matchPath      = Just nObjectPath
      , matchMember    = Just "ActionInvoked"
      }      

-- |Stop listening to DBus Notifications.
uninstallActionsHandler :: Client -> SignalHandler -> IO ()
uninstallActionsHandler = removeMatch

data Capability
  = ActionsCap | BodyCap | BodyHyperlinksCap | BodyImagesCap
  | BodyMarkupCap | IconMultiCap | IconStaticCap | SoundCap
  | UnknownCap String
  deriving (Eq, Read, Show, Generic)

readCapability :: String -> Capability
readCapability s = case s of
  "actions" -> ActionsCap
  "body" -> BodyCap
  "body-hyperlinks" -> BodyHyperlinksCap
  "body-images" -> BodyImagesCap
  "body-markup" -> BodyMarkupCap
  "icon-multi" -> IconMultiCap
  "icon-static" -> IconStaticCap
  "sound" -> SoundCap
  s' -> UnknownCap s'

nObjectPath :: ObjectPath
nObjectPath = "/org/freedesktop/Notifications"

nInterfaceName :: InterfaceName
nInterfaceName = "org.freedesktop.Notifications"

nBusName :: BusName
nBusName = "org.freedesktop.Notifications"

callNotificationMethod :: Client -> MemberName -> [Variant] -> IO MethodReturn
callNotificationMethod client methodName args =
  call_ client $ (methodCall nObjectPath nInterfaceName methodName)
  { methodCallDestination = Just nBusName
  , methodCallBody        = args
  }

timeoutInt :: Timeout -> Int32
timeoutInt Never = 0
timeoutInt Dependent = -1
timeoutInt (Milliseconds n)
  | n > 0     = n
  | otherwise = error "notification timeout not positive"

flattenBody :: Body -> String
flattenBody (Text s) = concatMap escape s
  where
    escape '>' = "&gt;"
    escape '<' = "&lt;"
    escape '&' = "&amp;"
    escape x = [x]
flattenBody (Bold b) = "<b>" ++ flattenBody b ++ "</b>"
flattenBody (Italic b) = "<i>" ++ flattenBody b ++ "</i>"
flattenBody (Underline b) = "<u>" ++ flattenBody b ++ "</u>"
flattenBody (Hyperlink h b) = "<a href=\"" ++ h ++ "\">" ++ flattenBody b ++ "</a>"
flattenBody (Img h alt) = "<img src=\"" ++ h ++ "\" alt=\"" ++ alt ++ "\"/>"
flattenBody (Concat b1 b2) = flattenBody b1 ++ flattenBody b2

actionsArray :: Actions -> [String]
actionsArray (Actions actions) = concatMap pairToList pairs
  where
    pairs = map (actionName &&& actionLabel) actions
    pairToList (a,b) = [a,b]

hintsDict :: [Hint] -> M.Map String Variant
hintsDict = M.fromList . map hint
  where
    hint :: Hint -> (String, Variant)
    hint (Urgency u) = ("urgency", toVariant (fromIntegral $ fromEnum u :: Word8))
    hint (Category c) = ("category", toVariant $ catName c)
    hint (ImagePath p) = ("image-path", toVariant $ iconString p)
    hint (SoundFile s) = ("sound-file", toVariant s)
    hint (SuppressSound b) = ("suppress-sound", toVariant b)
    hint (X x) = ("x", toVariant x)
    hint (Y y) = ("x", toVariant y)

-- HACK: Assumes the constructor for category foo.bar is FooBar and
-- categories have no capital letters
catName :: Category -> String
catName cat = catName' (show cat)
  where
    catName' [] = []
    catName' (c:cs) =
      map toLower $ c: (uncurry (++) . second ('.':) . span isLower $ cs)

getActionKey :: Signal -> Maybe (Notification, String)
getActionKey sig =
  readKey $ signalBody sig
  where
    readKey (aid:aname:[]) = do
      actId <- fromVariant @Word32 aid
      actName <- fromVariant @String aname
      return (Notification actId, actName)
    readKey _ = Nothing

runNotify
  :: IOE :> es
  => Client ->
  Eff (Notify : State ActionHandlers : Error NotifyError : es) a
  -> Eff es (Either (CallStack, NotifyError) (SignalHandler, a))
runNotify client m = run $
  (,) <$> installActionsHandler <*> m
  where
    run = runError
      . evalState (ActionHandlers mempty)
      . evalStaticRep (Notify client)
