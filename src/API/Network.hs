module API.Network
  ( getUpdatesRequest
  , sendMessageRequest
  , runGetUpdate
  , runSendMessage
  , answerCallback
  , latest
  , Timeout
  , Token(..)
  )
where

import qualified API.Telegram          as TG
import qualified Data.ByteString.Char8 as BC
import           Data.List
import           Network.HTTP.Simple

telegramHost :: BC.ByteString
telegramHost = "api.telegram.org"

type Timeout = Integer
type Host = BC.ByteString
type Method = BC.ByteString
type RequestMethod = BC.ByteString

newtype Token = Token { unToken :: BC.ByteString }

buildRequest :: Token -> RequestMethod -> Host -> Method -> Request
buildRequest token requestMethod host method =
  setRequestMethod requestMethod
    $ setRequestPath ("/" <> unToken token <> "/" <> method)
    $ setRequestHost host
    $ setRequestSecure True
    $ setRequestPort 443 defaultRequest

getUpdatesRequest :: Token -> Maybe Integer -> Maybe Timeout -> Request
getUpdatesRequest token offset timeout =
  let request = buildRequest token "GET" telegramHost "getUpdates"
  in  setRequestQueryString
        (maybe
          []
          (\t ->
            [ ("timeout", Just (BC.pack $ show t))
            , ("offset" , fmap (BC.pack . show) offset)
            ]
          )
          timeout
        ) -- TODO: this is very ugly, consider creating ad-hoc type with moniid properties and toQueryString func
        request

sendMessageRequest :: Token -> TG.OutgoingMessage -> Request
sendMessageRequest token m =
  setRequestBodyJSON m $ buildRequest token "POST" telegramHost "sendMessage"

answerCallback :: Token -> TG.CallbackAnswer -> Request
answerCallback token c = setRequestBodyJSON c
  $ buildRequest token "POST" telegramHost "answerCallbackQuery"

runGetUpdate :: Request -> IO (Maybe [TG.Update])
runGetUpdate request = do
  response <-
    httpJSONEither request :: IO
      (Response (Either JSONException TG.GetUpdatesResponse))
  case getResponseBody response of
    (Left  _             ) -> return Nothing
    (Right updateResponse) -> return $ Just $ TG.guResult updateResponse

runSendMessage :: Request -> IO (Response TG.Message)
runSendMessage = httpJSON

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

-- TODO: the order should be desc
-- TODO: return update, not the id
latest :: [TG.Update] -> Maybe Integer
latest = safeHead . sort . map TG.uId
