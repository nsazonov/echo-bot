module TelegramSpec where

import           Test.Hspec
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import Data.Maybe
import           API.Telegram

main :: IO ()
main = hspec spec

telegramMessage = "test/data/telegramMessage.json"

withJsonFile :: FilePath -> (B.ByteString -> IO ()) -> IO ()
withJsonFile p f = B.readFile p >>= f

getUpdates :: (GetUpdatesResponse -> IO ()) -> IO ()
getUpdates f = withJsonFile telegramMessage (f . fromJust . decode)

spec :: Spec
spec = do
  around getUpdates $ do
    describe "parse getUpdates json" $ do
      it "should be successful" $ \resp -> do        
        ok resp `shouldBe` True
      it "should have a list of result objects" $ \resp -> do        
        let rs = result resp
        length rs `shouldBe` 1
      it "should have text in the message" $ \resp -> do        
        let update = fromJust . text . fromJust . message . head $ result resp
        update `shouldBe` "/help"




