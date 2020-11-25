module TelegramSpec where

import qualified API.Telegram as TG
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

telegramMessage :: FilePath
telegramMessage = "test/data/telegramMessage.json"

withJsonFile :: FilePath -> (B.ByteString -> IO ()) -> IO ()
withJsonFile p f = B.readFile p >>= f

getUpdates :: (TG.GetUpdatesResponse -> IO ()) -> IO ()
getUpdates f = withJsonFile telegramMessage (f . fromJust . decode)

spec :: Spec
spec = do
  around getUpdates $ do
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
