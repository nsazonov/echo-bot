module Logger
  ( Level
  , Config(..)
  , withHandle
  , log
  , debug
  , info
  , warning
  , error
  )
where

import           Prelude                 hiding ( error
                                                , log
                                                )

data Level = Debug | Info | Warning | Error deriving (Eq, Ord, Show)
newtype Config = Config {level :: Level} deriving (Show)
newtype Handle = Handle {config :: Config} deriving (Show)

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle = undefined

log :: Handle -> Level -> String -> IO ()
log Handle {..} v x
  | v >= verbosity = putStrLn $ "[" ++ show v ++ "]:" ++ show x
  | otherwise      = return ()
  where verbosity = Debug

debug, info, warning, error :: Handle -> String -> IO ()
debug h = log h Debug
info h = log h Info
warning h = log h Warning
error h = log h Error

