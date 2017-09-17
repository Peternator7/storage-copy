module Main where

import qualified Lib
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do 
    putStrLn "Please enter a path:"
    path <- B.getLine
    putStrLn "Please enter a storage account:"
    storage <- B.getLine 
    putStrLn "Please enter a sas query:"
    sas <- B.getLine
    Lib.crawlDir path storage sas
