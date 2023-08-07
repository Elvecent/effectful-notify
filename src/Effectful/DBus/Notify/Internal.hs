module Effectful.DBus.Notify.Internal where

import                          Control.Arrow                (second, (&&&))
import                          Data.Char                    (isLower, toLower)
import                          Data.Foldable
import                          Data.Int
import                qualified Data.Map.Strict              as M
import                          Data.Word
import                          GHC.Generics

import                          Effectful
import {-# SOURCE #-}           Effectful.DBus.Notify.Action
import                          Effectful.Dispatch.Static
import                          Effectful.Internal.Utils

import                          DBus
import                          DBus.Client

type NotifyCtx = (Client, MVar' ActionHandlers)

data Notify :: Effect

type instance DispatchOf Notify = Static WithSideEffects

newtype instance StaticRep Notify = Notify NotifyCtx

-- |Action definition with a handler
data Action = Action
  { actionName    :: String
  , actionLabel   :: String
  , actionHandler :: IO ()
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
  { appName  :: String
  , appImage :: Maybe Icon
  , summary  :: String
  , body     :: Maybe Body
  , actions  :: Actions
  , hints    :: [Hint]
  , expiry   :: Timeout
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
iconString (File fp)   = "file://" ++ fp
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

data Capability
  = ActionsCap | BodyCap | BodyHyperlinksCap | BodyImagesCap
  | BodyMarkupCap | IconMultiCap | IconStaticCap | SoundCap
  | UnknownCap String
  deriving (Eq, Read, Show, Generic)

readCapability :: String -> Capability
readCapability s = case s of
  "actions"         -> ActionsCap
  "body"            -> BodyCap
  "body-hyperlinks" -> BodyHyperlinksCap
  "body-images"     -> BodyImagesCap
  "body-markup"     -> BodyMarkupCap
  "icon-multi"      -> IconMultiCap
  "icon-static"     -> IconStaticCap
  "sound"           -> SoundCap
  s'                -> UnknownCap s'

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
    escape x   = [x]
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
