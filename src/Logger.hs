-- | The logger interface module. It should not define a specific
-- implementation.
module Logger
  ( Handle (..),
    Level (..),
    debug,
    info,
    warning,
    error,
    (.<),
  )
where

import qualified Data.Text as T
import Prelude hiding
  ( error,
  )

-- | The logger handle. This is a public logger interface that can
-- have different implementations. You can use it everywhere.
newtype Handle m = Handle
  { hLowLevelLog :: Level -> T.Text -> m ()
  }

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

debug, info, warning, error :: Handle m -> T.Text -> m ()
debug h = hLowLevelLog h Debug
info h = hLowLevelLog h Info
warning h = hLowLevelLog h Warning
error h = hLowLevelLog h Error

-- | Concatenates a text and an instance of 'Show'. This is a
-- convenience function to make logger function applications more
-- concise:
--
-- > Log.logError (hLogger h) "The error code is " .< e
(.<) :: (Show a) => T.Text -> a -> T.Text
text .< a = text <> T.pack (show a)

infixr 7 .<
