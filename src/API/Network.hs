module API.Network
  ( fetchUpdates
  )
where

import           API.Telegram
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LC
import           Network.HTTP.Simple

host :: BC.ByteString
host = "api.telegram.org"

timeout :: BC.ByteString
timeout = "5"

type Timeout = BC.ByteString
type Token = BC.ByteString
type Host = BC.ByteString
type Method = BC.ByteString

buildRequest :: Token -> Host -> Method -> Timeout -> Request
buildRequest token host method timeout =
  setRequestMethod "GET"
    $ setRequestQueryString [("timeout", Just timeout)]
    $ setRequestPath (BC.append (BC.append (BC.append "/" token) "/") method)
    $ setRequestHost host
    $ setRequestSecure True
    $ setRequestPort 443
    $ defaultRequest

getUpdatesRequest :: Token -> IO (Response GetUpdatesResponse)
getUpdatesRequest token =
  httpJSON $ buildRequest token host "getUpdates" timeout

fetchUpdates :: Token -> IO ()
fetchUpdates token = do
  response <- getUpdatesRequest token
  print response
