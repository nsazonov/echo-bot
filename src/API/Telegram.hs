module API.Telegram
  ( GetUpdatesResponse(..)
  , Update(..)
  , Message(..)
  , OutgoingMessage(..)
  , Chat(..)
  )
where

import           Data.Aeson
import           Data.Text                     as T
import           GHC.Generics

data GetUpdatesResponse = GetUpdatesResponse {ok :: Bool, result :: [Update]} deriving (Show, Generic)
data Update = Update {update_id :: Integer, message :: Maybe Message} deriving (Show, Generic)
data Message = Message {message_id :: Integer, text :: Maybe T.Text} deriving (Show, Generic)
data OutgoingMessage = OutgoingMessage {chat_id :: Integer, text :: T.Text} deriving (Show, Generic)
newtype Chat = Chat {id :: Integer} deriving (Show, Generic)

instance FromJSON GetUpdatesResponse
instance FromJSON Update
instance FromJSON Message
instance FromJSON Chat

instance ToJSON OutgoingMessage
