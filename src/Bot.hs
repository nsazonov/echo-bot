module Bot
  ( main,
  )
where

import API.Network
import qualified API.Telegram as TG
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Read
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

data Command = Action T.Text | Help | Repeat | RepeatAnswer T.Text Int deriving (Show)

newtype Target = Target {unTarget :: Int} deriving (Show)

type ClientSettings = Map.Map TG.Chat Int

runLoop :: Handle -> Maybe Offset -> IO ()
runLoop handle = botLoop handle (cToken . hConfig $ handle) Map.empty

botLoop :: Handle -> Token -> ClientSettings -> Maybe Offset -> IO ()
botLoop handle token settings offset = do
  let timeout = Just $ Timeout 100
  Logger.debug (hLogger handle) ("Waiting " ++ show (fromJust timeout) ++ " seconds")
  response <- runGetUpdate (hLogger handle) $ getUpdatesRequest token offset timeout
  case response of
    Left e -> do
      Logger.error (hLogger handle) e
      botLoop handle token settings offset
    Right updates -> do
      Logger.debug (hLogger handle) ("Received " ++ show (length updates) ++ " updates")
      Logger.debug (hLogger handle) updates
      result <- processUpdates handle updates
      case result of
        Nothing -> do
          Logger.debug (hLogger handle) ("No updates or disconnected." :: String)
          botLoop handle token settings offset
        Just newOffset -> do
          Logger.info (hLogger handle) newOffset
          botLoop handle token settings (Just $ succ newOffset)

processUpdates :: Handle -> [TG.Update] -> IO (Maybe Offset)
processUpdates handle updates = do
  let callBacks = mapMaybe fromCallbackQuery updates
  let others = mapMaybe fromMessage updates
  Logger.debug (hLogger handle) (("Number of callbacks: " ++ show (length callBacks)) :: String)
  Logger.debug (hLogger handle) (("Number of other commands: " ++ show (length others)) :: String)
  mapM_ (uncurry (execute handle)) $ callBacks ++ others
  Logger.debug (hLogger handle) ("Finished processing the updates" :: String)
  return $ nextOffset updates

nextOffset :: [TG.Update] -> Maybe Offset
nextOffset updates = case latest updates of
  Nothing -> Nothing
  Just update -> Just $ Offset $ TG.uId update

fromMessage :: TG.Update -> Maybe (Target, Command)
fromMessage update = TG.uMessage update >>= \message -> TG.mText message >>= \text -> Just (Target $ TG.cId $ TG.mChat message, fromText text)
  where
    fromText t
      | "/help" `T.isPrefixOf` t = Help
      | "/start" `T.isPrefixOf` t = Help
      | "/repeat" `T.isPrefixOf` t = Repeat
      | otherwise = Action t

fromCallbackQuery :: TG.Update -> Maybe (Target, Command)
fromCallbackQuery update =
  TG.uCallbackQuery update >>= \callback ->
    parseData (TG.cqData callback) >>= \repeatNumber ->
      TG.cqMessage callback >>= \message ->
        Just (Target $ TG.cId $ TG.mChat message, RepeatAnswer (TG.cqId callback) repeatNumber)

parseData :: T.Text -> Maybe Int
parseData t = case decimal t of
  Left _ -> Nothing
  Right v -> Just $ fromInteger $ fst v

execute :: Handle -> Target -> Command -> IO ()
execute handle _ (RepeatAnswer queryId repeatNumber) = do
  let token = cToken $ hConfig handle
  let message = TG.CallbackAnswer {caQueryId = queryId, caText = T.pack $ "Repeat number is set to " ++ show repeatNumber}
  Logger.debug (hLogger handle) ("Will send callback answer" ++ show message :: String)
  _ <- runAnswerCallback (hLogger handle) $ answerCallback token message
  Logger.debug (hLogger handle) ("Answer callback sent" :: String)
  return ()
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
