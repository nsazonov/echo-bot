module Logger
  ( Level (..),
    Config (..),
    Handle,
    withHandle,
    log,
    debug,
    info,
    warning,
    error,
  )
where

import Prelude hiding
  ( error,
    log,
  )

data Level = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

newtype Config = Config {cLevel :: Level} deriving (Show)

newtype Handle = Handle {hConfig :: Config} deriving (Show)

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle c f = f Handle {hConfig = c}

log :: Show s => Handle -> Level -> s -> IO ()
log Handle {..} v x
  | v >= verbosity = putStrLn $ "[" ++ show v ++ "]:" ++ show x
  | otherwise = return ()
  where
    verbosity = Debug

debug, info, warning, error :: Show s => Handle -> s -> IO ()
debug h = log h Debug
info h = log h Info
warning h = log h Warning
error h = log h Error
