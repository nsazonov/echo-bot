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
      runLoop
        Handle
          { hConfig =
              Config
                { cToken = Token $ BC.pack token,
                  cGreetings = "Hello, this is Echo bot."
                },
            hLogger = logger
          }
        Nothing
      where
    _ -> do
      putStrLn $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

data Config = Config {cToken :: Token, cGreetings :: T.Text} deriving (Show)

data Handle = Handle {hConfig :: Config, hLogger :: Logger.Handle} deriving (Show)

data Command = Action T.Text | Help | Repeat deriving (Show)

newtype Target = Target {unTarget :: Integer} deriving (Show)

runLoop :: Handle -> Maybe Offset -> IO ()
runLoop handle = botLoop handle (cToken . hConfig $ handle)

botLoop :: Handle -> Token -> Maybe Offset -> IO ()
botLoop handle token offset = do
  let timeout = Just $ Timeout 100
  Logger.debug (hLogger handle) ("Waiting " ++ show (fromJust timeout) ++ " seconds")
  response <- runGetUpdate (hLogger handle) $ getUpdatesRequest token offset timeout
  case response of
    Left e -> do
      Logger.error (hLogger handle) e
      botLoop handle token offset
    Right updates -> do
      result <- processUpdates handle updates
      case result of
        Nothing -> do
          Logger.debug (hLogger handle) ("No updates or disconnected." :: String)
          botLoop handle token offset
        Just newOffset -> do
          Logger.info (hLogger handle) newOffset
          botLoop handle token (Just $ succ newOffset)

processUpdates :: Handle -> [TG.Update] -> IO (Maybe Offset)
processUpdates handle updates = do
  mapM_ (uncurry (execute handle)) $ mapMaybe toCommand updates
  return $ nextOffset updates

nextOffset :: [TG.Update] -> Maybe Offset
nextOffset updates = case latest updates of
  Nothing -> Nothing
  Just update -> Just $ Offset $ TG.uId update

toCommand :: TG.Update -> Maybe (Target, Command)
toCommand update = TG.uMessage update >>= \message -> TG.mText message >>= \text -> Just (Target $ TG.cId $ TG.mChat message, fromText text)

fromText :: T.Text -> Command
fromText t
  | "/help" `T.isPrefixOf` t = Help
  | "/repeat" `T.isPrefixOf` t = Repeat
  | otherwise = Action t

execute :: Handle -> Target -> Command -> IO ()
execute handle target (Action t) = do
  let token = cToken $ hConfig handle
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = t, omReplyMarkup = Nothing}
  result <- runSendMessage (hLogger handle) $ sendMessageRequest token message
  case result of
    Left e -> Logger.error (hLogger handle) e
    Right _ -> Logger.info (hLogger handle) ("Message has been sent" :: String)
execute handle _ command = Logger.info (hLogger handle) command
