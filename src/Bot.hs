module Bot
  ( main,
  )
where

import API.Network
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [token] -> botLoop (Token $ BC.pack token) (Just 10)
    _ -> do
      putStrLn $ "Usage: " ++ progName ++ " <conf>"
      exitFailure

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
