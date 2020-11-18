module LibSpec where

import           Test.Hspec

main :: IO ()
main = hspec spec

telegramMessage = "test/data/telegramMessage.json"

withJsonFile :: FilePath -> (String -> IO ()) -> IO ()
withJsonFile p f = readFile p >>= f

spec :: Spec
spec = do
  around (withJsonFile telegramMessage) $ do
    describe "parse Telegram json" $ do
      it "parses message object" $ \s -> do
        null s `shouldBe` False



