{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Network
  ( answerCallback,
    getUpdatesRequest,
    sendMessageRequest,
    runAnswerCallback,
    runGetUpdate,
    runSendMessage,
    latest,
    Timeout (..),
    Token (..),
    Offset (..),
  )
where

import qualified API.Telegram as TG
import Control.Exception (try, displayException)
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import qualified Logger
import Network.HTTP.Simple

telegramHost :: Host
telegramHost = Host "api.telegram.org"

data RequestMethod = GET | POST deriving (Show)

newtype Token = Token {unToken :: BC.ByteString} deriving (Show)

newtype Host = Host {unHost :: BC.ByteString}

newtype Timeout = Timeout {unTimeout :: Integer}

instance Show Timeout where
  show t = show $ unTimeout t

newtype Offset = Offset {unOffset :: Int} deriving (Enum)

instance Show Offset where
  show = show . unOffset

data APIMethod = GetUpdates | SendMessage | AnswerCallbackQuery

instance Show APIMethod where
  show GetUpdates = "getUpdates"
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

runAnswerCallback :: Logger.Handle -> Request -> IO (Either String ())
runAnswerCallback logger request = do
  response <- try $ httpJSONEither request :: IO (Either HttpException (Response (Either JSONException ())))
  case response of
    Left e -> do
      Logger.error logger $ displayException (e :: HttpException) -- ^ TODO: Handle timeout exception only
      return $ Left $ show e
    Right _ -> return $ Right ()

runGetUpdate :: Logger.Handle -> Request -> IO (Either String [TG.Update])
runGetUpdate logger request = do
  response <-
    try $ httpJSONEither request ::
      IO
        (Either HttpException (Response (Either JSONException TG.GetUpdatesResponse)))
  case response of
    Left e -> do
      Logger.error logger $ displayException (e :: HttpException) -- ^ TODO: Handle timeout exception only
      return $ Left $ show e
    Right resp -> do
      Logger.debug logger ("getUpdate status: " ++ show (getResponseStatusCode resp))
      case getResponseBody resp of
        (Left exception) -> return $ Left $ show exception
        (Right updateResponse) -> return (Right $ TG.guResult updateResponse)

runSendMessage :: Logger.Handle -> Request -> IO (Either String TG.Message)
runSendMessage logger request = do
  response <- httpJSONEither request :: IO (Response (Either JSONException TG.SendMessageResponse))
  Logger.debug logger ("sendMessage response:" :: String)
  Logger.debug logger response
  case getResponseBody response of
    (Left exception) -> return $ Left $ show exception
    (Right smResponse) -> return (Right $ TG.smResult smResponse)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

latest :: [TG.Update] -> Maybe TG.Update
latest = safeHead . sort
