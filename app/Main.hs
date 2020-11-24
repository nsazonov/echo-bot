module Main where
import           System.Environment
import qualified API.Network                   as N

main :: IO ()
main = do
  args <- getArgs
  if null args then putStrLn "Usage:" else print (head args)

botLoop :: N.Token -> Maybe Integer -> IO ()
botLoop token offset = do
  putStrLn "Waiting..."
  response <- N.runGetUpdate $ N.getUpdatesRequest token offset (Just 5)
  case response of
    Nothing -> do 
      putStrLn "Nothing"      
      botLoop token offset
    (Just updates) -> do
      case N.latest updates of
        Nothing -> do 
          putStrLn "No updates!"          
          botLoop token offset
        Just newOffset -> do 
          putStrLn $ "Offset: " ++ show newOffset
          botLoop token (Just (newOffset + 1))
  
