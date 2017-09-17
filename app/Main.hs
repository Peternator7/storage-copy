module Main where

import qualified Lib

main :: IO ()
main = do 
    putStrLn "Please enter a path:"
    path <- getLine
    Lib.crawlDir path
