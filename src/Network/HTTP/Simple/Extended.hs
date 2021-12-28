module Network.HTTP.Simple.Extended (isTimeout) where

import Network.HTTP.Client.Conduit (HttpExceptionContent (..))
import Network.HTTP.Simple (HttpException (..))

isTimeout :: HttpException -> Bool
isTimeout (HttpExceptionRequest _ ResponseTimeout) = True
isTimeout (HttpExceptionRequest _ _) = False
isTimeout _ = False
