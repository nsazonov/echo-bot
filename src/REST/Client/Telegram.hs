module REST.Client.Telegram where

import qualified API.Telegram as TG
import qualified Data.Text as T
import Logger ((.<))
import qualified Logger
import REST.Network as Network
import REST.Types

sendText :: Token -> Target -> T.Text -> IO (Either NetworkError ())
sendText token target text = do
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = text, omReplyMarkup = Nothing}
  result <- Network.run $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message)
  case result of
    Left e -> return $ Left e
    Right _ -> return $ Right ()

sendRepeatInput :: Token -> Target -> T.Text -> IO (Either NetworkError ())
sendRepeatInput token target statusMessage = do
  let kb =
        TG.InlineKeyboardMarkup
          [ [ TG.InlineKeyboardButton {ikbText = "1", ikbCallBackData = "1"},
              TG.InlineKeyboardButton {ikbText = "2", ikbCallBackData = "2"},
              TG.InlineKeyboardButton {ikbText = "3", ikbCallBackData = "3"},
              TG.InlineKeyboardButton {ikbText = "4", ikbCallBackData = "4"},
              TG.InlineKeyboardButton {ikbText = "5", ikbCallBackData = "5"}
            ]
          ]
  let message = TG.OutgoingMessage {omChatId = unTarget target, omText = statusMessage, omReplyMarkup = Just kb}
  result <- Network.run $ request token $ TG.SendMessage message :: IO (Either Network.NetworkError TG.Message)
  case result of
    Left e -> return $ Left e
    Right _ -> return $ Right ()

sendAnswer :: Token -> Logger.Handle IO -> T.Text -> Int -> IO (Either NetworkError ())
sendAnswer token logger queryId repeatNumber = do
  let message =
        TG.CallbackAnswer
          { caQueryId = queryId,
            caText = T.pack $ "Repeat number is set to " ++ show repeatNumber,
            caShowAlert = True
          }
  Logger.debug logger $ "Will send callback answer" .< message
  result <- Network.run $ request token $ TG.AnswerCallbackQuery message :: IO (Either Network.NetworkError ()) -- TODO: log errors
  Logger.debug logger "Answer callback sent"
  case result of
    Left e -> return $ Left e
    Right _ -> return $ Right ()

getUpdates :: Token -> Maybe Offset -> Maybe Timeout -> IO (Either NetworkError [TG.Update])
getUpdates token offset timeout = do
  response <- Network.run $ request token $ TG.GetUpdates offset timeout :: IO (Either NetworkError TG.GetUpdatesResponse)
  case response of
    Left e -> return $ Left e
    Right resp -> return (Right $ TG.guResult resp)
