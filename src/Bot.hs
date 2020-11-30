module Bot
  ( main,
  )
where

import API.Network
import qualified API.Telegram as TG
import Control.Monad (replicateM_)
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
                  cGreetings = "Hello, this is Echo bot.",
                  cDefaultRepeatNumber = 3
                },
            hLogger = logger
          }
        Nothing
      where
    _ -> do
      putStrLn $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

data Config = Config {cToken :: Token, cGreetings :: T.Text, cDefaultRepeatNumber :: Int} deriving (Show)

data Handle = Handle {hConfig :: Config, hLogger :: Logger.Handle} deriving (Show)

data Command = Action T.Text | Help | Repeat deriving (Show)

newtype Target = Target {unTarget :: Int} deriving (Show)

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
      Logger.debug (hLogger handle) ("Received " ++ show (length updates) ++ " updates")
      Logger.debug (hLogger handle) updates
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
  Logger.debug (hLogger handle) ("Finished processing the updates" :: String)
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
  | "/start" `T.isPrefixOf` t = Help
  | "/repeat" `T.isPrefixOf` t = Repeat
  | otherwise = Action t

execute :: Handle -> Target -> Command -> IO ()
execute handle target Repeat = do
  let token = cToken $ hConfig handle
  let repeatNumber = cDefaultRepeatNumber $ hConfig handle
  let statusMessage = "Current repeat number is " ++ show repeatNumber
  let kb = TG.InlineKeyboardMarkup [[TG.InlineKeyboardButton {ikbText = "1", ikbCallBackData = "1"}, TG.InlineKeyboardButton {ikbText = "2", ikbCallBackData = "2"}, TG.InlineKeyboardButton {ikbText = "3", ikbCallBackData = "3"}]]
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = T.pack statusMessage, omReplyMarkup = Just kb}
  _ <- runSendMessage (hLogger handle) $ sendMessageRequest token message -- TODO: log errors
  return ()
execute handle target Help = do
  let token = cToken $ hConfig handle
  let greetingsMessage = cGreetings $ hConfig handle
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = greetingsMessage, omReplyMarkup = Nothing}
  _ <- runSendMessage (hLogger handle) $ sendMessageRequest token message -- TODO: log errors
  return ()
execute handle target (Action t) = do
  let token = cToken $ hConfig handle
  let times = cDefaultRepeatNumber $ hConfig handle
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = t, omReplyMarkup = Nothing}
  replicateM_ times $ runSendMessage (hLogger handle) $ sendMessageRequest token message -- TODO: log failed messages here  
