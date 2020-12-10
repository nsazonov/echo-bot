module REST.Network
  ( run,
    Timeout (..),
    Token (..),
    NetworkError (..),
  )
where

import qualified API.Telegram as TG
import Control.Exception
import qualified Data.Aeson as A
import qualified Logger
import Network.HTTP.Simple
import Network.HTTP.Simple.Extended
import REST.Types

data NetworkError = PollTimeout | HttpError HttpException | ResponseError JSONException deriving (Show)

run :: (A.FromJSON a) => Logger.Handle -> Request -> IO (Either NetworkError a)
run _ r = do
  res <- try $ httpJSONEither r :: (A.FromJSON a) => IO (Either HttpException (Response (Either JSONException a)))
  case res of
    Left e -> if isTimeout e then return $ Left PollTimeout else return $ Left (HttpError e)
    Right v -> case getResponseBody v of
      Left ex -> return $ Left (ResponseError ex)
      Right value -> return $ Right value
