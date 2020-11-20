module API.Network
  ( getUpdatesRequest
  , sendMessageRequest
  , runGetUpdate
  , runSendMessage
  , Timeout
  )
where

import           API.Telegram
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LC
import           Data.Maybe
import           Network.HTTP.Simple

host :: BC.ByteString
host = "api.telegram.org"

defaultTimeout :: Integer
defaultTimeout = 10

type Timeout = Integer
type Token = BC.ByteString
type Host = BC.ByteString
type Method = BC.ByteString
type RequestMethod = BC.ByteString

buildRequest :: Token -> RequestMethod -> Host -> Method -> Request
buildRequest token requestMethod host method =
  setRequestMethod requestMethod
    $ setRequestPath (BC.append (BC.append (BC.append "/" token) "/") method)
    $ setRequestHost host
    $ setRequestSecure True
    $ setRequestPort 443 defaultRequest

getUpdatesRequest :: Token -> Maybe Timeout -> IO (Response GetUpdatesResponse)
getUpdatesRequest token timeout =
  let request = buildRequest token "GET" host "getUpdates"
  in  httpJSON $ setRequestQueryString
        (maybe [] (\t -> [("timeout", Just (BC.pack $ show t))]) timeout)
        request

runGetUpdate :: Request -> IO (Response GetUpdatesResponse)
runGetUpdate = httpJSON

sendMessageRequest :: Token -> OutgoingMessage -> Request
sendMessageRequest token message =
  setRequestBodyJSON message $ buildRequest token "POST" host "sendMessage"

runSendMessage :: Request -> IO (Response Message)
runSendMessage = httpJSON
