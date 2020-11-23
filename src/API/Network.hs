module API.Network
  ( getUpdatesRequest
  , sendMessageRequest
  , runGetUpdate
  , runSendMessage
  , answerCallback
  , Timeout
  )
where

import           API.Telegram
import qualified Data.ByteString.Char8         as BC
import           Network.HTTP.Simple

telegramHost :: BC.ByteString
telegramHost = "api.telegram.org"

type Timeout = Integer
type Token = BC.ByteString
type Host = BC.ByteString
type Method = BC.ByteString
type RequestMethod = BC.ByteString

buildRequest :: Token -> RequestMethod -> Host -> Method -> Request
buildRequest token requestMethod host method =
  setRequestMethod requestMethod
    $ setRequestPath ("/" <> token <> "/" <> method)
    $ setRequestHost host
    $ setRequestSecure True
    $ setRequestPort 443 defaultRequest

getUpdatesRequest :: Token -> Maybe Timeout -> Request
getUpdatesRequest token timeout =
  let request = buildRequest token "GET" telegramHost "getUpdates"
  in  setRequestQueryString
        (maybe [] (\t -> [("timeout", Just (BC.pack $ show t))]) timeout)
        request

sendMessageRequest :: Token -> OutgoingMessage -> Request
sendMessageRequest token m =
  setRequestBodyJSON m $ buildRequest token "POST" telegramHost "sendMessage"

answerCallback :: Token -> CallbackAnswer -> Request
answerCallback token c = setRequestBodyJSON c $ buildRequest token "POST" telegramHost "answerCallbackQuery"

runGetUpdate :: Request -> IO (Maybe Update)
runGetUpdate request = do
    response <-
      httpJSONEither request :: IO
        (Response (Either JSONException GetUpdatesResponse))
    case getResponseBody response of
      (Left  _             ) -> return Nothing
      (Right updateResponse) -> return $ safeHead $ result updateResponse
  
runSendMessage :: Request -> IO (Response Message)
runSendMessage = httpJSON

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x
