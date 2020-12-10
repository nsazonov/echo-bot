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
    
--runGetUpdate :: Logger.Handle -> Request -> IO (Either String [TG.Update])
--runGetUpdate logger r = do
--  response <-
--    try $ httpJSONEither r ::
--      IO
--        (Either HttpException (Response (Either JSONException TG.GetUpdatesResponse)))
--  case response of
--    Left e -> do
--      Logger.error logger $ displayException (e :: HttpException) -- TODO: Handle timeout exception only
--      return $ Left $ show e
--    Right resp -> do
--      Logger.debug logger ("getUpdate status: " ++ show (getResponseStatusCode resp))
--      case getResponseBody resp of
--        (Left exception) -> return $ Left $ show exception
--        (Right updateResponse) -> return (Right $ TG.guResult updateResponse)
        
--runAnswerCallback :: Logger.Handle -> Request -> IO (Either String ())
--runAnswerCallback logger r = do
--  response <- try $ httpJSONEither r :: IO (Either HttpException (Response (Either JSONException ())))
--  case response of
--    Left e -> do
--      Logger.error logger $ displayException (e :: HttpException) -- TODO: Handle timeout exception only
--      return $ Left $ show e
--    Right _ -> return $ Right ()

--runSendMessage :: Logger.Handle -> Request -> IO (Either String TG.Message)
--runSendMessage logger r = do
--  response <- httpJSONEither r :: IO (Response (Either JSONException TG.SendMessageResponse))
--  Logger.debug logger ("sendMessage response:" :: String)
--  Logger.debug logger response
--  case getResponseBody response of
--    (Left exception) -> return $ Left $ show exception
--    (Right smResponse) -> return (Right $ TG.smResult smResponse)


  
