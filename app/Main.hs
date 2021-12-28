module Main where

import qualified Bot
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  Bot.main progName args
  return ()
