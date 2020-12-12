module REST.Client.Telegram where

import qualified API.Telegram as TG
import qualified Data.Text as T
import qualified Logger
import REST.Network as Network
import REST.Types

sendText :: Token -> Logger.Handle -> Target -> T.Text -> IO (Either NetworkError ())
sendText token logger target text = do
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = text, omReplyMarkup = Nothing}
  result <- Network.run logger $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message)
  case result of
    Left e -> return $ Left e
    Right _ -> return $ Right ()

sendRepeatInput :: Token -> Logger.Handle -> Target -> T.Text -> IO (Either NetworkError ())
sendRepeatInput token logger target statusMessage = do
  let kb = TG.InlineKeyboardMarkup [[TG.InlineKeyboardButton {ikbText = "1", ikbCallBackData = "1"}, TG.InlineKeyboardButton {ikbText = "2", ikbCallBackData = "2"}, TG.InlineKeyboardButton {ikbText = "3", ikbCallBackData = "3"}]]
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = statusMessage, omReplyMarkup = Just kb}
  result <- Network.run logger $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message)
  case result of
    Left e -> return $ Left e
    Right _ -> return $ Right ()

sendAnswer :: Token -> Logger.Handle -> T.Text -> Int -> IO (Either NetworkError ())
sendAnswer token logger queryId repeatNumber = do
  let message = TG.CallbackAnswer {caQueryId = queryId, caText = T.pack $ "Repeat number is set to " ++ show repeatNumber}
  Logger.debug logger ("Will send callback answer" ++ show message :: String)
  result <- Network.run logger $ request token $ TG.AnswerCallbackQuery message :: IO (Either Network.NetworkError ()) -- TODO: log errors
  Logger.debug logger ("Answer callback sent" :: String)
  case result of
    Left e -> return $ Left e
    Right _ -> return $ Right ()

getUpdates :: Token -> Logger.Handle -> Maybe Offset -> Maybe Timeout -> IO (Either NetworkError [TG.Update])
getUpdates token logger offset timeout = do
  response <- Network.run logger $ request token $ TG.GetUpdates offset timeout :: IO (Either NetworkError TG.GetUpdatesResponse)
  case response of
    Left e -> return $ Left e
    Right resp -> return (Right $ TG.guResult resp)
