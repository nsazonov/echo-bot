module Bot
  ( main,
  )
where

import qualified API.Telegram as TG
import Commands
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Logger
import qualified REST.Client.Telegram as Telegram
import REST.Types
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

runLoop :: Config -> Logger.Handle -> Maybe Offset -> IO ()
runLoop config logger = botLoop config logger Map.empty

botLoop ::
  Config ->
  Logger.Handle ->
  ClientSettings ->
  Maybe Offset ->
  IO ()
botLoop config logger settings offset = do
  let timeout = Just $ Timeout 100
  let token = cToken config
  Logger.debug logger ("Waiting " ++ show (fromJust timeout) ++ " seconds")
  response <- Telegram.getUpdates token logger offset timeout
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
          Logger.debug logger ("New offset is: " ++ show (succ newOffset) :: String)
          botLoop config logger newSettings (Just $ succ newOffset)

processUpdates :: Config -> Logger.Handle -> ClientSettings -> [TG.Update] -> IO (Maybe (Offset, ClientSettings))
processUpdates config logger settings updates = do
  let callBacks = mapMaybe fromCallbackQuery updates
  let others = mapMaybe fromMessage updates
  Logger.debug logger (("Number of callbacks: " ++ show (length callBacks)) :: String)
  Logger.debug logger (("Number of other commands: " ++ show (length others)) :: String)
  mapM_ (uncurry (executeCommand (cMessenger config) config logger settings)) $ callBacks ++ others
  Logger.debug logger ("Finished processing the updates" :: String)
  return $ (,Map.empty) <$> TG.nextOffset updates
