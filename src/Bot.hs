module Bot
  ( main,
  )
where

import qualified API.Network as Network
import qualified API.Telegram as TG
import API.Types
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BC
import Data.List.Extended
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

type ClientSettings = Map.Map Target Int

runLoop :: Handle -> Maybe TG.Offset -> IO ()
runLoop handle = botLoop handle Map.empty

botLoop :: Handle -> ClientSettings -> Maybe TG.Offset -> IO ()
botLoop handle settings offset = do
  let timeout = Just $ Timeout 100
  let token = cToken . hConfig $ handle
  Logger.debug (hLogger handle) ("Waiting " ++ show (fromJust timeout) ++ " seconds")
  response <- Network.run (hLogger handle) $ request token $ TG.GetUpdates offset timeout
  case response of
    Left e -> do
      Logger.error (hLogger handle) e
      botLoop handle settings offset
    Right updates -> do
      Logger.debug (hLogger handle) ("Received " ++ show (length updates) ++ " updates")
      Logger.debug (hLogger handle) updates
      result <- processUpdates handle settings updates
      case result of
        Nothing -> do
          Logger.debug (hLogger handle) ("No updates or disconnected." :: String)
          botLoop handle settings offset
        Just (newOffset, newSettings) -> do
          Logger.info (hLogger handle) newOffset
          botLoop handle newSettings (Just $ succ newOffset)

processUpdates :: Handle -> ClientSettings -> [TG.Update] -> IO (Maybe (TG.Offset, ClientSettings))
processUpdates handle settings updates = do
  let callBacks = mapMaybe fromCallbackQuery updates
  let others = mapMaybe fromMessage updates
  Logger.debug (hLogger handle) (("Number of callbacks: " ++ show (length callBacks)) :: String)
  Logger.debug (hLogger handle) (("Number of other commands: " ++ show (length others)) :: String)
  mapM_ (uncurry (execute handle settings)) $ callBacks ++ others
  Logger.debug (hLogger handle) ("Finished processing the updates" :: String)
  return $ (,Map.empty) <$> nextOffset updates

nextOffset :: [TG.Update] -> Maybe TG.Offset
nextOffset updates = case latest updates of
  Nothing -> Nothing
  Just update -> Just $ TG.Offset $ TG.uId update

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

execute :: Handle -> ClientSettings -> Target -> Command -> IO ClientSettings
execute handle settings _ (RepeatAnswer queryId repeatNumber) = do
  let token = cToken $ hConfig handle
  let message = TG.CallbackAnswer {caQueryId = queryId, caText = T.pack $ "Repeat number is set to " ++ show repeatNumber}
  Logger.debug (hLogger handle) ("Will send callback answer" ++ show message :: String)
  _ <- Network.run (hLogger handle) $ request token $ TG.AnswerCallbackQuery message :: IO (Either Network.NetworkError ()) -- TODO: log errors
  Logger.debug (hLogger handle) ("Answer callback sent" :: String)
  return settings
execute handle settings target Repeat = do
  let token = cToken $ hConfig handle
  let repeatNumber = cDefaultRepeatNumber $ hConfig handle
  let statusMessage = "Current repeat number is " ++ show repeatNumber
  let kb = TG.InlineKeyboardMarkup [[TG.InlineKeyboardButton {ikbText = "1", ikbCallBackData = "1"}, TG.InlineKeyboardButton {ikbText = "2", ikbCallBackData = "2"}, TG.InlineKeyboardButton {ikbText = "3", ikbCallBackData = "3"}]]
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = T.pack statusMessage, omReplyMarkup = Just kb}
  _ <- Network.run (hLogger handle) $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message) -- TODO: log errors
  return settings
execute handle settings target Help = do
  let token = cToken $ hConfig handle
  let greetingsMessage = cGreetings $ hConfig handle
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = greetingsMessage, omReplyMarkup = Nothing}
  _ <- Network.run (hLogger handle) $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message) -- TODO: log errors
  return settings
execute handle settings target (Action t) = do
  let token = cToken $ hConfig handle
  let times = cDefaultRepeatNumber $ hConfig handle
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = t, omReplyMarkup = Nothing}
  replicateM_ times (Network.run (hLogger handle) $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message)) -- TODO: log failed messages here
  return settings
