module Bot
  ( main,
  )
where

import API.Network

main :: IO ()
main = putStrLn "Running Bot..."

botLoop :: Token -> Maybe Integer -> IO ()
botLoop token offset = do
  putStrLn "Waiting..."
  response <- runGetUpdate $ getUpdatesRequest token offset (Just $ Timeout 5)
  case response of
    Nothing -> do
      putStrLn "Nothing"
      botLoop token offset
    (Just updates) -> do
      case latest updates of
        Nothing -> do
          putStrLn "No updates!"
          botLoop token offset
        Just newOffset -> do
          putStrLn $ "Offset: " ++ show newOffset
          botLoop token (Just (newOffset + 1))
