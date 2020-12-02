module API.Network
  ( runAnswerCallback,
    runGetUpdate,
    runSendMessage,
    latest,
    Timeout (..),
    Token (..),
  )
where

import qualified API.Telegram as TG
import API.Types
import Control.Exception (displayException, try)
import Data.List (sort)
import qualified Logger
import Network.HTTP.Simple

runAnswerCallback :: Logger.Handle -> Request -> IO (Either String ())
runAnswerCallback logger r = do
  response <- try $ httpJSONEither r :: IO (Either HttpException (Response (Either JSONException ())))
  case response of
    Left e -> do
      Logger.error logger $ displayException (e :: HttpException) -- TODO: Handle timeout exception only
      return $ Left $ show e
    Right _ -> return $ Right ()

runGetUpdate :: Logger.Handle -> Request -> IO (Either String [TG.Update])
runGetUpdate logger r = do
  response <-
    try $ httpJSONEither r ::
      IO
        (Either HttpException (Response (Either JSONException TG.GetUpdatesResponse)))
  case response of
    Left e -> do
      Logger.error logger $ displayException (e :: HttpException) -- TODO: Handle timeout exception only
      return $ Left $ show e
    Right resp -> do
      Logger.debug logger ("getUpdate status: " ++ show (getResponseStatusCode resp))
      case getResponseBody resp of
        (Left exception) -> return $ Left $ show exception
        (Right updateResponse) -> return (Right $ TG.guResult updateResponse)

runSendMessage :: Logger.Handle -> Request -> IO (Either String TG.Message)
runSendMessage logger r = do
  response <- httpJSONEither r :: IO (Response (Either JSONException TG.SendMessageResponse))
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
