module TelegramSpec where

import qualified API.Telegram as TG
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

telegramMessage :: FilePath
telegramMessage = "test/data/telegramTextMessage.json"

telegramPollMessage :: FilePath
telegramPollMessage = "test/data/telegramPollMessage.json"

telegramCallback :: FilePath
telegramCallback = "test/data/telegramCallback.json"

withJsonFile :: FilePath -> (B.ByteString -> IO ()) -> IO ()
withJsonFile p f = B.readFile p >>= f

getUpdates :: FilePath -> (TG.GetUpdatesResponse -> IO ()) -> IO ()
getUpdates file f = withJsonFile file (f . fromJust . decode)

spec :: Spec
spec = do
  around (getUpdates telegramPollMessage) $ do
    describe "parse updates for new poll message" $ do
      it "shoud be successful" $ \resp -> do
        TG.guOk resp `shouldBe` True
  around (getUpdates telegramCallback) $ do
    describe "parse updates for inline keyboard callback" $ do
      it "shoud be successful" $ \resp -> do
        TG.guOk resp `shouldBe` True
  around (getUpdates telegramMessage) $ do
    describe "parse getUpdates json" $ do
      it "should be successful" $ \resp -> do
        TG.guOk resp `shouldBe` True
      it "should have a list of result objects" $ \resp -> do
        let rs = TG.guResult resp
        length rs `shouldBe` 1
      it "should have text in the message" $ \resp -> do
        let update =
              fromJust . TG.mText . fromJust . TG.uMessage . head $ TG.guResult resp
        update `shouldBe` "/help"
