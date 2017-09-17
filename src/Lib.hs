{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( crawlDir
    ) 
    where

-- Needed for our normal process
import System.IO (withBinaryFile)
import Control.Monad
import Data.Traversable (forM)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (listDirectory, doesFileExist)

-- Needed for http requests.
import Control.Exception (throwIO)
import Control.Monad
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

instance MonadHttp IO where
    handleHttpException = throwIO

baseUri :: Text
baseUri = ""
sas :: Text
sas     = ""

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
        uploadFile path
    else 
        recurseDir path

uploadFile :: String -> IO ()
uploadFile path = do 
    -- Format (Sun, 17 Sep 2017 17:11:02 GTC)
    time <- getCurrentTime >>= return . B.pack . formatTime defaultTimeLocale "%a, %d %b %Y %T GTC"
    let p = tail path
    let headers = (header "x-ms-date" time)
    -- putStrLn ("Uploading: " ++ p ++ " @ "++ time)
    bs <- req PUT (https baseUri /~ p /~ sas) (ReqBodyFile path) bsResponse headers
    return ()
    