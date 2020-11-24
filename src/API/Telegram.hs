module API.Telegram
  ( GetUpdatesResponse(..)
  , Update(..)
  , Message(..)
  , OutgoingMessage(..)
  , InlineKeyboardMarkup(..)
  , InlineKeyboardButton(..)
  , CallbackAnswer(..)
  )
where

import qualified Data.Aeson                    as A
import           Data.Text                     as T

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
    A.object ["chat_id" A..= omChatId p, "text" A..= omText p, "reply_markup" A..= omReplyMarkup p]

instance A.ToJSON InlineKeyboardMarkup where
  toJSON p = A.object ["inline_keyboard" A..= ikmKeyboard p]

instance A.ToJSON InlineKeyboardButton where
  toJSON p = A.object ["text" A..= ikbText p, "callback_data" A..= ikbCallBackData p]

instance A.FromJSON Message where
  parseJSON = A.withObject "FromJSON API.Telegram.Message" $ \o -> do
    i <- o A..: "message_id"
    t <- o A..: "text"
    return Message { mId = i, mText = t }

instance A.FromJSON Update where
  parseJSON = A.withObject "FromJSON API.Telegram.Update" $ \o -> do
    i <- o A..: "update_id"
    m <- o A..:? "message"
    return Update { uId = i, uMessage = m }

instance A.FromJSON GetUpdatesResponse where
  parseJSON = A.withObject "FromJSON API.Telegram.GetUpdatesResponse" $ \o ->
    do
      k <- o A..: "ok"
      r <- o A..: "result"
      return GetUpdatesResponse { guOk = k, guResult = r }

instance A.FromJSON OutgoingMessage where
  parseJSON = A.withObject "FromJSON API.Telegram.OutgoingMessage" $ \o -> do
    ci <- o A..: "chat_id"
    t  <- o A..: "text"
    r  <- o A..: "reply_markup"
    return OutgoingMessage { omChatId = ci, omText = t, omReplyMarkup = r }

instance A.FromJSON InlineKeyboardMarkup where
  parseJSON =
    A.withObject "FromJSON API.Telegram.InlineKeyboardMarkup" $ \o -> do
      m <- o A..: "inline_keyboard"
      return InlineKeyboardMarkup { ikmKeyboard = m }

instance A.FromJSON InlineKeyboardButton where
  parseJSON =
    A.withObject "FromJSON API.Telegram.InlineKeyboardButton" $ \o -> do
      t <- o A..: "text"
      c <- o A..: "callback_data"
      return InlineKeyboardButton { ikbText = t, ikbCallBackData = c }

instance A.FromJSON CallbackAnswer where
  parseJSON = A.withObject "FromJSON API.Telegram.CallbackAnswer" $ \o -> do
    i <- o A..: "callback_query_id"
    t <- o A..: "text"
    return CallbackAnswer { caQueryId = i, caText = t }

