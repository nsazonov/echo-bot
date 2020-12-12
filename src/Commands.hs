module Commands where

import qualified API.Telegram as TG
import qualified Data.Text as T
import Data.Text.Read
import REST.Types

data Command = Action T.Text | Help | Repeat | RepeatAnswer T.Text Int deriving (Show)

fromMessage :: TG.Update -> Maybe (Target, Command)
fromMessage update =
  TG.uMessage update
    >>= \message ->
      TG.mText message
        >>= \text -> Just (Target $ TG.cId $ TG.mChat message, fromText text)
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
