module Bot
  ( main,
  )
where

import API.Network
import qualified Data.ByteString.Char8 as BC
import qualified Logger
import System.Environment
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [token] -> Logger.withHandle (Logger.Config Logger.Debug) $ \logger ->
      runLoop Handle {hConfig = Config (Token $ BC.pack token), hLogger = logger} Nothing
      where
    _ -> do
      putStrLn $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

newtype Config = Config {cToken :: Token} deriving (Show)

data Handle = Handle {hConfig :: Config, hLogger :: Logger.Handle} deriving (Show)

runLoop :: Handle -> Maybe Offset -> IO ()
runLoop handle = botLoop (hLogger handle) (cToken . hConfig $ handle)

botLoop :: Logger.Handle -> Token -> Maybe Offset -> IO ()
botLoop logger token offset = do
  Logger.info logger "Waiting..."
  response <- runGetUpdate $ getUpdatesRequest token offset (Just $ Timeout 5)
  case response of
    Nothing -> do
      Logger.info logger "Nothing"
      botLoop logger token offset
    (Just updates) -> do
      case latest updates of
        Nothing -> do
          Logger.info logger "No updates!"
          botLoop logger token offset
        Just newOffset -> do
          Logger.info logger $ "Offset: " ++ show newOffset
          botLoop logger token (Just $ Offset $ newOffset + 1)
