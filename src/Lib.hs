module Lib
    ( crawlDir
    ) 
    where

import Control.Monad
import Data.Traversable (forM)
import System.Directory (listDirectory, doesFileExist)

crawlDir :: String -> IO ()
crawlDir = recurseDir

recurseDir :: String -> IO ()
recurseDir path = do
        dirs <- listDirectory path
        let mapped = map (\p -> path ++ "/" ++ p) dirs 
        forM mapped recursePath
        return ()

recursePath :: String -> IO ()
recursePath path = do
    isFile <- doesFileExist path
    if isFile then
        putStrLn path
    else 
        recurseDir path
