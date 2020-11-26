module API.Network
  ( getUpdatesRequest,
    sendMessageRequest,
    runGetUpdate,
    runSendMessage,
    answerCallback,
    latest,
    Timeout (..),
    Token (..),
    Offset (..),
  )
where

import qualified API.Telegram as TG
import qualified Data.ByteString.Char8 as BC
import Data.List
import Network.HTTP.Simple

telegramHost :: Host
telegramHost = Host "api.telegram.org"

data RequestMethod = GET | POST deriving (Show)

newtype Token = Token {unToken :: BC.ByteString} deriving (Show)

newtype Host = Host {unHost :: BC.ByteString}

newtype Timeout = Timeout {unTimeout :: Integer}

newtype Offset = Offset {unOffset :: Integer} deriving (Show)

data APIMethod = GetUpdates | SendMessage | AnswerCallbackQuery

instance Show APIMethod where
  show GetUpdates = "getUpdate"
  show SendMessage = "sendMessage"
  show AnswerCallbackQuery = "answerCallbackQuery"

buildRequest :: Token -> RequestMethod -> Host -> APIMethod -> Request
buildRequest token requestMethod host method =
  setRequestMethod (BC.pack $ show requestMethod) $
    setRequestPath ("/" <> unToken token <> "/" <> BC.pack (show method)) $
      setRequestHost (unHost host) $
        setRequestSecure True $
          setRequestPort 443 defaultRequest

getUpdatesRequest :: Token -> Maybe Offset -> Maybe Timeout -> Request
getUpdatesRequest token offset timeout =
  let request = buildRequest token GET telegramHost GetUpdates
   in setRequestQueryString
        ( maybe
            []
            ( \t ->
                [ ("timeout", Just (BC.pack $ show (unTimeout t))),
                  ("offset", fmap (BC.pack . show . unOffset) offset)
                ]
            )
            timeout
        ) -- TODO: this is very ugly, consider creating ad-hoc type with moniid properties and toQueryString func
        request

sendMessageRequest :: Token -> TG.OutgoingMessage -> Request
sendMessageRequest token m =
  setRequestBodyJSON m $ buildRequest token POST telegramHost SendMessage

answerCallback :: Token -> TG.CallbackAnswer -> Request
answerCallback token c =
  setRequestBodyJSON c $
    buildRequest token POST telegramHost AnswerCallbackQuery

runGetUpdate :: Request -> IO (Maybe [TG.Update])
runGetUpdate request = do
  response <-
    httpJSONEither request ::
      IO
        (Response (Either JSONException TG.GetUpdatesResponse))
  case getResponseBody response of
    (Left _) -> return Nothing
    (Right updateResponse) -> return $ Just $ TG.guResult updateResponse

runSendMessage :: Request -> IO (Response TG.Message)
runSendMessage = httpJSON

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- TODO: the order should be desc
-- TODO: return update, not the id
latest :: [TG.Update] -> Maybe Integer
latest = safeHead . sort . map TG.uId
