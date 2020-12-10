module Bot
  ( main,
  )
where

import qualified API.Telegram as TG
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BC
import Data.List.Extended
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Read
import qualified Logger
import qualified REST.Network as Network
import REST.Types
import qualified REST.Client.Telegram as Telegram
import System.Environment
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [token] -> Logger.withHandle (Logger.Config Logger.Debug) $ \logger ->
      runLoop
        Config
          { cToken = Token $ BC.pack token,
            cGreetings = "Hello, this is Echo bot.",
            cDefaultRepeatNumber = 3,
            cMessenger = Telegram
          }
        logger
        Nothing
      where
    _ -> do
      putStrLn $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

data Config = Config {cToken :: Token, cGreetings :: T.Text, cDefaultRepeatNumber :: Int, cMessenger :: Messenger} deriving (Show)

data Handle = Handle {hConfig :: Config, hLogger :: Logger.Handle} deriving (Show)

data Command = Action T.Text | Help | Repeat | RepeatAnswer T.Text Int deriving (Show)

type ClientSettings = Map.Map Target Int -- TODO: rename this

data Messenger = VK | Telegram | Test deriving (Show)

class Client a where
  executeCommand :: a -> Config -> Logger.Handle -> ClientSettings -> Target -> Command -> IO ClientSettings  

instance Client Messenger where
  executeCommand :: Messenger -> Config -> Logger.Handle -> ClientSettings -> Target -> Command -> IO ClientSettings
  executeCommand VK _ _ _ _ _ = undefined
  executeCommand Test _ _ _ _ _ = undefined
  executeCommand Telegram config logger settings _ (RepeatAnswer queryId repeatNumber) = do
    let token = cToken config
    _ <- Telegram.sendAnswer token logger queryId repeatNumber -- TODO: log errors
    return settings
  executeCommand Telegram config logger settings target Repeat = do
    let token = cToken config
    let repeatNumber = cDefaultRepeatNumber config
    let statusMessage = "Current repeat number is " ++ show repeatNumber
    _ <- Telegram.sendRepeatInput token logger target $ T.pack statusMessage -- TODO: log errors
    return settings
  executeCommand Telegram config logger settings target Help = do
    let token = cToken config
    let greetingsMessage = cGreetings config
    _ <- Telegram.sendText token logger target greetingsMessage -- TODO: log errors
    return settings
  executeCommand Telegram config logger settings target (Action t) = do
    let token = cToken config
    let times = cDefaultRepeatNumber config    
    replicateM_ times (Telegram.sendText token logger target t) -- TODO: log failed messages here
    return settings

runLoop :: Config -> Logger.Handle -> Maybe TG.Offset -> IO ()
runLoop config logger = botLoop config logger Map.empty

botLoop :: Config -> Logger.Handle -> ClientSettings -> Maybe TG.Offset -> IO ()
botLoop config logger settings offset = do
  let timeout = Just $ Timeout 100
  let token = cToken config
  Logger.debug logger ("Waiting " ++ show (fromJust timeout) ++ " seconds")
  response <- Network.run logger $ request token $ TG.GetUpdates offset timeout
  case response of
    Left e -> do
      Logger.error logger e
      botLoop config logger settings offset
    Right updates -> do
      Logger.debug logger ("Received " ++ show (length updates) ++ " updates")
      Logger.debug logger updates
      result <- processUpdates config logger settings updates
      case result of
        Nothing -> do
          Logger.debug logger ("No updates or disconnected." :: String)
          botLoop config logger settings offset
        Just (newOffset, newSettings) -> do
          Logger.info logger newOffset
          botLoop config logger newSettings (Just $ succ newOffset)

processUpdates :: Config -> Logger.Handle -> ClientSettings -> [TG.Update] -> IO (Maybe (TG.Offset, ClientSettings))
processUpdates config logger settings updates = do
  let callBacks = mapMaybe fromCallbackQuery updates
  let others = mapMaybe fromMessage updates
  Logger.debug logger (("Number of callbacks: " ++ show (length callBacks)) :: String)
  Logger.debug logger (("Number of other commands: " ++ show (length others)) :: String)
  mapM_ (uncurry (executeCommand (cMessenger config) config logger settings)) $ callBacks ++ others
  Logger.debug logger ("Finished processing the updates" :: String)
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
