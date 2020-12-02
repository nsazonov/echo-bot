module API.Types where

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple (Request)
    
newtype Token = Token {unToken :: BC.ByteString} deriving (Show)

newtype Host = Host {unHost :: BC.ByteString}

newtype Timeout = Timeout {unTimeout :: Integer}

instance Show Timeout where
  show t = show $ unTimeout t
  
data RequestMethod = GET | POST deriving (Show) -- TODO: move this to Aeson.Extended

class EndpointBuilder a where
  request :: Token -> a -> Request



