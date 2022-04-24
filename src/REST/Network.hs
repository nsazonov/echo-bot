module REST.Network
  ( run,
    Timeout (..),
    Token (..),
    NetworkError (..),
  )
where

import Control.Exception
import qualified Data.Aeson as A
import Network.HTTP.Simple
import Network.HTTP.Simple.Extended
import REST.Types

data NetworkError = PollTimeout | HttpError HttpException | ResponseError JSONException deriving (Show)

run :: (A.FromJSON a) => Request -> IO (Either NetworkError a)
run r = do
  res <- try $ httpJSONEither r :: (A.FromJSON a) => IO (Either HttpException (Response (Either JSONException a)))
  case res of
    Left e -> if isTimeout e then return $ Left PollTimeout else return $ Left (HttpError e)
    Right v -> case getResponseBody v of
      Left ex -> return $ Left (ResponseError ex)
      Right value -> return $ Right value
