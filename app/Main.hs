module Main where

import qualified Lib
import Control.Monad.Trans.Maybe

main :: IO ()
main = do 
    putStrLn "Please enter a path:"
    path <- getLine
    Lib.crawlDir path
