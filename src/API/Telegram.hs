module API.Telegram
  ( GetUpdatesResponse(..)
  , Update(..)
  , Message(..)
  , OutgoingMessage(..)
  , Chat(..)  
  , InlineKeyboardMarkup(..)
  , InlineKeyboardButton(..)
  , CallbackAnswer(..)
  )
where

import           Data.Aeson
import           Data.Text                     as T
import           GHC.Generics

data GetUpdatesResponse = GetUpdatesResponse {ok :: Bool, result :: [Update]} deriving (Show, Generic)
data Update = Update {update_id :: Integer, message :: Maybe Message} deriving (Show, Generic)
data Message = Message {message_id :: Integer, text :: Maybe T.Text} deriving (Show, Generic)
data OutgoingMessage = OutgoingMessage {chat_id :: Integer, text :: T.Text, reply_markup :: InlineKeyboardMarkup} deriving (Show, Generic)
data CallbackAnswer = CallbackAnswer {callback_query_id :: T.Text, text :: T.Text} deriving (Show, Generic)
newtype Chat = Chat {id :: Integer} deriving (Show, Generic)
newtype InlineKeyboardMarkup = InlineKeyboardMarkup {inline_keyboard :: [[InlineKeyboardButton]]} deriving (Show, Generic)
data InlineKeyboardButton = InlineKeyboardButton {text :: T.Text, callback_data :: T.Text} deriving (Show, Generic)

instance FromJSON GetUpdatesResponse
instance FromJSON Update
instance FromJSON Message
instance FromJSON Chat

instance ToJSON OutgoingMessage
instance ToJSON CallbackAnswer
instance ToJSON InlineKeyboardMarkup
instance ToJSON InlineKeyboardButton
