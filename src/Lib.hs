{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( crawlDir
    ) 
    where

-- Needed for our normal process
import System.IO (withBinaryFile)
import Control.Monad
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Traversable (forM)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (listDirectory, doesFileExist, makeAbsolute, withCurrentDirectory, getCurrentDirectory)

-- Needed for http requests.
import Control.Exception (throwIO, try)
import Data.Monoid ((<>))
import Data.Maybe(fromJust)
import Network.HTTP.Req
import Data.ByteString(concat)
import qualified Data.ByteString.Char8 as B

instance MonadHttp IO where
    handleHttpException = throwIO

getContainerName :: String -> B.ByteString
getContainerName = B.pack . (map toLower) . last . (splitOn "/")

isHidden :: String -> Bool
isHidden = ('.' ==) . head

crawlDir :: B.ByteString -> B.ByteString -> B.ByteString -> IO ()
crawlDir path storage sas = 
    withCurrentDirectory (B.unpack $ path) (do
        p <- getCurrentDirectory
        files <- recurseDir p
        let container = getContainerName p
        (try $ createContainer container storage sas) :: IO (Either HttpException ())
        forM_ files (\file -> uploadFile p file container storage sas))

recurseDir :: String -> IO [String]
recurseDir path = do
    dirs <- listDirectory path
    -- Filter out any paths starting with a '.'
    let absolutePaths = map (\file -> path <> "/" <> file) . filter (not . isHidden) $ dirs 
    mapM recursePath absolutePaths >>= return . Prelude.concat

recursePath :: String -> IO [String]
recursePath path = doesFileExist path >>= recursePath'
    where recursePath' True  = return [path]
          recursePath' False = recurseDir path

createContainer :: B.ByteString -> B.ByteString -> B.ByteString -> IO ()
createContainer name storage sas = do 
    time <- getFormattedTime
    let uri = ["https://", storage , ".blob.core.windows.net/", name, "?" , sas, "&restype=container"]
    let (url, options) = fromJust . parseUrlHttps . B.concat $ uri
    let headers = (header "x-ms-date" time) <> options
    B.putStrLn "Creating a new container"
    req PUT url NoReqBody bsResponse headers
    return ()

getFormattedTime :: IO B.ByteString
getFormattedTime = getCurrentTime >>= return . B.pack . formatTime defaultTimeLocale "%a, %d %b %Y %T GTC"

uploadFile :: String -> String -> B.ByteString -> B.ByteString -> B.ByteString -> IO ()
uploadFile basePath fullPath container storage sas = do 
    time <- getFormattedTime
    let p = drop (length basePath) fullPath
    putStrLn $ "Uploading file: " ++ p
    let uri = ["https://", storage , ".blob.core.windows.net/", container , B.pack p, "?" , sas ]
    let (url, options) = fromJust . parseUrlHttps . B.concat $ uri
    let headers = (header "x-ms-date" time)             <>
                  (header "x-ms-blob-type" "BlockBlob") <>
                   options
    req PUT url (ReqBodyFile fullPath) bsResponse headers
    return ()
