module Bot
  ( main,
  )
where

import API.Network
import qualified Data.ByteString.Char8 as BC
import qualified Logger
import System.Environment
import System.Exit (exitFailure)
import Data.Maybe (fromJust)

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
  let timeout = Just $ Timeout 5
  Logger.info logger ("Waiting " ++ show (fromJust timeout) ++ " seconds")  
  response <- runGetUpdate $ getUpdatesRequest token offset timeout
  case response of
    Left e -> do
      Logger.error logger e
      botLoop logger token offset
    Right updates -> do
      case latest updates of
        Nothing -> do
          Logger.info logger "No updates!"
          botLoop logger token offset
        Just newOffset -> do
          Logger.info logger $ "Offset: " ++ show newOffset
          botLoop logger token (Just $ Offset $ newOffset + 1)
