module Bot
  ( main,
  )
where

import API.Network
import qualified API.Telegram as TG
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import qualified Data.Text as T
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

data Command = Action T.Text | Help | Repeat deriving (Show)

runLoop :: Handle -> Maybe Offset -> IO ()
runLoop handle = botLoop (hLogger handle) (cToken . hConfig $ handle)

botLoop :: Logger.Handle -> Token -> Maybe Offset -> IO ()
botLoop logger token offset = do
  let timeout = Just $ Timeout 100
  Logger.debug logger ("Waiting " ++ show (fromJust timeout) ++ " seconds")
  response <- runGetUpdate $ getUpdatesRequest token offset timeout
  case response of
    Left e -> do
      Logger.error logger e
      botLoop logger token offset
    Right updates -> do
      result <- processUpdates logger updates
      case result of
        Nothing -> do
          Logger.debug logger "No updates or disconnected."
          botLoop logger token offset
        Just newOffset -> do
          Logger.info logger $ "Offset: " ++ show newOffset
          botLoop logger token (Just $ succ newOffset)

processUpdates :: Logger.Handle -> [TG.Update] -> IO (Maybe Offset)
processUpdates logger updates = do
  mapM_ (execute logger) $ mapMaybe fromUpdate updates
  return $ nextOffset updates

nextOffset :: [TG.Update] -> Maybe Offset
nextOffset updates = case latest updates of
  Nothing -> Nothing
  Just update -> Just $ Offset $ TG.uId update

fromUpdate :: TG.Update -> Maybe Command
fromUpdate update = TG.uMessage update >>= TG.mText >>= Just . fromText

fromText :: T.Text -> Command
fromText t
  | "/help" `T.isPrefixOf` t = Help
  | "/repeat" `T.isPrefixOf` t = Repeat
  | otherwise = Action t

execute :: Logger.Handle -> Command -> IO ()
execute logger command = Logger.info logger $ show command
