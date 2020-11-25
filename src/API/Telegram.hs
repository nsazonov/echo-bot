module API.Telegram
  ( GetUpdatesResponse (..),
    Update (..),
    Message (..),
    OutgoingMessage (..),
    InlineKeyboardMarkup (..),
    InlineKeyboardButton (..),
    CallbackAnswer (..),
  )
where

import qualified Data.Aeson as A
import Data.Text as T

data GetUpdatesResponse = GetUpdatesResponse {guOk :: Bool, guResult :: [Update]} deriving (Show)

data Update = Update {uId :: Integer, uMessage :: Maybe Message} deriving (Show)

data Message = Message {mId :: Integer, mText :: Maybe T.Text} deriving (Show)

data OutgoingMessage = OutgoingMessage {omChatId :: Integer, omText :: T.Text, omReplyMarkup :: InlineKeyboardMarkup} deriving (Show)

data CallbackAnswer = CallbackAnswer {caQueryId :: T.Text, caText :: T.Text} deriving (Show)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {ikmKeyboard :: [[InlineKeyboardButton]]} deriving (Show)

data InlineKeyboardButton = InlineKeyboardButton {ikbText :: T.Text, ikbCallBackData :: T.Text} deriving (Show)

instance A.ToJSON CallbackAnswer where
  toJSON p =
    A.object ["callback_query_id" A..= caQueryId p, "text" A..= caText p]

instance A.ToJSON OutgoingMessage where
  toJSON p =
    A.object
      [ "chat_id" A..= omChatId p,
        "text" A..= omText p,
        "reply_markup" A..= omReplyMarkup p
      ]

instance A.ToJSON InlineKeyboardMarkup where
  toJSON p = A.object ["inline_keyboard" A..= ikmKeyboard p]

instance A.ToJSON InlineKeyboardButton where
  toJSON p =
    A.object ["text" A..= ikbText p, "callback_data" A..= ikbCallBackData p]

instance A.FromJSON Message where
  parseJSON = A.withObject "FromJSON API.Telegram.Message" $ \o -> do
    mId <- o A..: "message_id"
    mText <- o A..: "text"
    return Message {..}

instance A.FromJSON Update where
  parseJSON = A.withObject "FromJSON API.Telegram.Update" $ \o -> do
    uId <- o A..: "update_id"
    uMessage <- o A..:? "message"
    return Update {..}

instance A.FromJSON GetUpdatesResponse where
  parseJSON = A.withObject "FromJSON API.Telegram.GetUpdatesResponse" $ \o ->
    do
      guOk <- o A..: "ok"
      guResult <- o A..: "result"
      return GetUpdatesResponse {..}

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
    return CallbackAnswer {..}
