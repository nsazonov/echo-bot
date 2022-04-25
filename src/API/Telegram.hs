module API.Telegram
  ( GetUpdatesResponse (..),
    TelegramEndpoint (..),
    Offset (..),
    SendMessageResponse (..),
    Update (..),
    Message (..),
    Poll (..),
    Chat (..),
    OutgoingMessage (..),
    InlineKeyboardMarkup (..),
    InlineKeyboardButton (..),
    CallbackAnswer (..),
    CallbackQuery (..),
    nextOffset,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import Data.List.Extended
import qualified Data.Text as T
import Network.HTTP.Simple
import REST.Types

telegramHost :: Host
telegramHost = Host "api.telegram.org"

data APIMethod = APIGetUpdates | APISendMessage | APIAnswerCallbackQuery

instance Show APIMethod where
  show APIGetUpdates = "getUpdates"
  show APISendMessage = "sendMessage"
  show APIAnswerCallbackQuery = "answerCallbackQuery"

buildRequest :: Token -> RequestMethod -> Host -> BC.ByteString -> Request
buildRequest token requestMethod host method =
  setRequestMethod (BC.pack $ show requestMethod) $
    setRequestPath ("/" <> unToken token <> "/" <> method) $
      setRequestHost (unHost host) $
        setRequestSecure True $
          setRequestPort 443 defaultRequest

nextOffset :: [Update] -> Maybe Offset
nextOffset updates = case latest updates of
  Nothing -> Nothing
  Just update -> Just $ Offset $ uId update

data TelegramEndpoint
  = GetUpdates (Maybe Offset) (Maybe Timeout)
  | SendMessage OutgoingMessage
  | AnswerCallbackQuery CallbackAnswer

instance EndpointBuilder TelegramEndpoint where
  request token (GetUpdates offset timeout) =
    let r = buildRequest token GET telegramHost $ BC.pack $ show APIGetUpdates
     in setRequestQueryString
          ( maybe
              []
              ( \t ->
                  [ ("timeout", Just (BC.pack $ show (unTimeout t))),
                    ("offset", fmap (BC.pack . show . unOffset) offset)
                  ]
              )
              timeout
          ) -- TODO: this is very ugly, consider creating ad-hoc type with monoid properties and toQueryString func
          r
  request token (SendMessage m) = setRequestBodyJSON m $ buildRequest token POST telegramHost $ BC.pack $ show APISendMessage
  request token (AnswerCallbackQuery a) = setRequestBodyJSON a $ buildRequest token POST telegramHost $ BC.pack $ show APIAnswerCallbackQuery

data GetUpdatesResponse = GetUpdatesResponse
  { guOk :: Bool,
    guResult :: [Update]
  }
  deriving (Show)

data SendMessageResponse = SendMessageResponse
  { smOk :: Bool,
    smResult :: Message
  }
  deriving (Show)

data Update = Update
  { uId :: Int,
    uMessage :: Maybe Message,
    uCallbackQuery :: Maybe CallbackQuery
  }
  deriving (Show)

data Poll = Poll
  { pId :: T.Text,
    pQuestion :: Maybe T.Text
  }
  deriving (Show)

data Message = Message
  { mId :: Int,
    mText :: Maybe T.Text,
    mPoll :: Maybe Poll,
    mChat :: Chat
  }
  deriving (Show)

newtype Chat = Chat
  { cId :: Int
  }
  deriving (Show)

data OutgoingMessage = OutgoingMessage
  { omChatId :: Int,
    omText :: T.Text,
    omReplyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving (Show)

data CallbackAnswer = CallbackAnswer
  { caQueryId :: T.Text,
    caText :: T.Text,
    caShowAlert :: Bool
  }
  deriving (Show)

data CallbackQuery = CallbackQuery
  { cqId :: T.Text,
    cqData :: T.Text,
    cqMessage :: Maybe Message
  }
  deriving (Show)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { ikmKeyboard :: [[InlineKeyboardButton]]
  }
  deriving (Show)

data InlineKeyboardButton = InlineKeyboardButton
  { ikbText :: T.Text,
    ikbCallBackData :: T.Text
  }
  deriving (Show)

instance Eq Update where
  (==) a b = uId a == uId b

instance Ord Update where
  compare a b = uId a `compare` uId b

instance A.ToJSON CallbackAnswer where
  toJSON p =
    A.object ["callback_query_id" A..= caQueryId p, "text" A..= caText p, "show_alert" A..= caShowAlert p]

instance A.ToJSON OutgoingMessage where
  toJSON p =
    A.object $
      filter
        ((/= A.Null) . snd)
        [ "chat_id" A..= omChatId p,
          "text" A..= omText p,
          "reply_markup" A..= omReplyMarkup p
        ]

instance A.ToJSON InlineKeyboardMarkup where
  toJSON p = A.object ["inline_keyboard" A..= ikmKeyboard p]

instance A.ToJSON InlineKeyboardButton where
  toJSON p =
    A.object ["text" A..= ikbText p, "callback_data" A..= ikbCallBackData p]

instance A.FromJSON Chat where
  parseJSON = A.withObject "FromJSON API.Telegram.Chat" $ \o -> do
    cId <- o A..: "id"
    return Chat {..}

instance A.FromJSON Poll where
  parseJSON = A.withObject "FromJSON API.Telegram.Poll" $ \o -> do
    pId <- o A..: "id"
    pQuestion <- o A..: "question"
    return Poll {..}

instance A.FromJSON Message where
  parseJSON = A.withObject "FromJSON API.Telegram.Message" $ \o -> do
    mId <- o A..: "message_id"
    mText <- o A..:? "text"
    mPoll <- o A..:? "poll"
    mChat <- o A..: "chat"
    return Message {..}

instance A.FromJSON Update where
  parseJSON = A.withObject "FromJSON API.Telegram.Update" $ \o -> do
    uId <- o A..: "update_id"
    uMessage <- o A..:? "message"
    uCallbackQuery <- o A..:? "callback_query"
    return Update {..}

instance A.FromJSON GetUpdatesResponse where
  parseJSON = A.withObject "FromJSON API.Telegram.GetUpdatesResponse" $ \o ->
    do
      guOk <- o A..: "ok"
      guResult <- o A..: "result"
      return GetUpdatesResponse {..}

instance A.FromJSON SendMessageResponse where
  parseJSON = A.withObject "FromJSON API.Telegram.SendMessageResponse" $ \o ->
    do
      smOk <- o A..: "ok"
      smResult <- o A..: "result"
      return SendMessageResponse {..}

instance A.FromJSON OutgoingMessage where
  parseJSON = A.withObject "FromJSON API.Telegram.OutgoingMessage" $ \o -> do
    omChatId <- o A..: "chat_id"
    omText <- o A..: "text"
    omReplyMarkup <- o A..: "reply_markup"
    return OutgoingMessage {..}

instance A.FromJSON InlineKeyboardMarkup where
  parseJSON =
    A.withObject "FromJSON API.Telegram.InlineKeyboardMarkup" $ \o -> do
      ikmKeyboard <- o A..: "inline_keyboard"
      return InlineKeyboardMarkup {..}

instance A.FromJSON InlineKeyboardButton where
  parseJSON =
    A.withObject "FromJSON API.Telegram.InlineKeyboardButton" $ \o -> do
      ikbText <- o A..: "text"
      ikbCallBackData <- o A..: "callback_data"
      return InlineKeyboardButton {..}

instance A.FromJSON CallbackAnswer where
  parseJSON = A.withObject "FromJSON API.Telegram.CallbackAnswer" $ \o -> do
    caQueryId <- o A..: "callback_query_id"
    caText <- o A..: "text"
    caShowAlert <- o A..: "show_alert"
    return CallbackAnswer {..}

instance A.FromJSON CallbackQuery where
  parseJSON = A.withObject "FromJSON API.Telegram.CallbackQuery" $ \o -> do
    cqId <- o A..: "id"
    cqData <- o A..: "data"
    cqMessage <- o A..:? "message"
    return CallbackQuery {..}
