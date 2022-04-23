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
import Logger ((.<))
import qualified Logger
import qualified Logger.Impl
import qualified REST.Client.Telegram as Telegram
import REST.Types
import System.Exit (exitFailure)
import System.IO

type ProgramName = String

type ProgramArgs = [String]

main :: ProgramName -> ProgramArgs -> IO ()
main progName args = do
  let loggerConfig = Logger.Impl.Config { confFileHandle = System.IO.stderr, confMinLevel=Logger.Debug }
  case args of
    [token] -> Logger.Impl.withHandle loggerConfig $ \logger ->
      runLoop
        BotConfig
          { cToken = Token $ BC.pack token,
            cGreetings = "Hello, this is Echo bot. Play with me.",
            cDefaultRepeatNumber = 3,
            cFrontEnd = Telegram
          }
        logger
    _ -> do
      putStrLn $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

data BotConfig = BotConfig
  { cToken :: Token,
    cGreetings :: T.Text,
    cDefaultRepeatNumber :: Int,
    cFrontEnd :: FrontEnd
  }
  deriving (Show)

type ClientSettings = Map.Map Target Int -- TODO: rename this

data FrontEnd = Telegram | Test deriving (Show)

executeCommand ::
  BotConfig ->
  Logger.Handle IO ->
  ClientSettings ->
  Target ->
  Command ->
  IO ClientSettings
executeCommand config logger settings target (RepeatAnswer queryId repeatNumber) = do
  let token = cToken config
  let newSetting = Map.insert target repeatNumber settings
  _ <- Telegram.sendAnswer token logger queryId repeatNumber -- TODO: log errors
  return newSetting
executeCommand config logger settings target Repeat = do
  let token = cToken config
  let repeatNumber = Map.findWithDefault (cDefaultRepeatNumber config) target settings
  let statusMessage = "Current repeat number is " ++ show repeatNumber
  _ <- Telegram.sendRepeatInput token logger target $ T.pack statusMessage -- TODO: log errors
  return settings
executeCommand config logger settings target Help = do
  let token = cToken config
  let greetingsMessage = cGreetings config
  _ <- Telegram.sendText token logger target greetingsMessage -- TODO: log errors
  return settings
executeCommand config logger settings target (Action t) = do
  let token = cToken config
  let times = cDefaultRepeatNumber config
  replicateM_ times (Telegram.sendText token logger target t) -- TODO: log failed messages here
  return settings

runLoop :: BotConfig -> Logger.Handle IO -> IO ()
runLoop config logger = botLoop config logger Map.empty Nothing

botLoop ::
  BotConfig ->
  Logger.Handle IO ->
  ClientSettings ->
  Maybe Offset ->
  IO ()
botLoop config logger settings offset = do
  let timeout = Just $ Timeout 100
  let token = cToken config
  Logger.debug logger $ "Waiting " .< fromJust timeout <> " seconds."
  response <- Telegram.getUpdates token logger offset timeout
  case response of
    Left e -> do
      Logger.error logger $ "" .< e
      botLoop config logger settings offset
    Right updates -> do
      Logger.debug logger $ "Received " .< length updates <> " updates."
      Logger.debug logger $ "" .< updates
      result <- processUpdates config logger settings updates
      case result of
        Nothing -> do
          Logger.debug logger "No updates or disconnected."
          botLoop config logger settings offset
        Just (newOffset, newSettings) -> do
          Logger.debug logger $ "New offset is: " .< succ newOffset
          botLoop config logger newSettings (Just $ succ newOffset)

processUpdates ::
  BotConfig ->
  Logger.Handle IO ->
  ClientSettings ->
  [TG.Update] ->
  IO (Maybe (Offset, ClientSettings))
processUpdates config logger settings updates = do
  let callBacks = mapMaybe fromCallbackQuery updates
  let others = mapMaybe fromMessage updates
  Logger.debug logger $ "Number of callbacks: " .< length callBacks
  Logger.debug logger $ "Number of other commands: " .< length others
  commandResults <- mapM (uncurry (executeCommand config logger settings)) $ callBacks ++ others
  let newSettings = mconcat commandResults
  Logger.debug logger "Finished processing the updates"
  return $ (,newSettings) <$> TG.nextOffset updates
