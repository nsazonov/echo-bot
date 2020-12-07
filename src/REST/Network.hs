module REST.Network
  ( run,
    runAnswerCallback,
    runGetUpdate,
    runSendMessage,
    Timeout (..),
    Token (..),
    NetworkError (..),
  )
where

import qualified API.Telegram as TG
import REST.Types
import Control.Exception
import qualified Data.Aeson as A
import qualified Logger
import Network.HTTP.Simple
import Network.HTTP.Simple.Extended

data NetworkError = PollTimeout | HttpError HttpException | ResponseError JSONException deriving (Show)

run :: (A.FromJSON a) => Logger.Handle -> Request -> IO (Either NetworkError a)
run _ r = do
  res <- try $ httpJSONEither r :: (A.FromJSON a) => IO (Either HttpException (Response (Either JSONException a)))
  case res of
    Left e -> if isTimeout e then return $ Left PollTimeout else return $ Left (HttpError e)
    Right v -> case getResponseBody v of
      Left ex -> return $ Left (ResponseError ex)
      Right value -> return $ Right value

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
