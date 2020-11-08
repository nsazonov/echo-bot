module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if null args then putStrLn "Usage:"
                 else print (head args)    
