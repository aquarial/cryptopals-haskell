{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NetworkIO where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import           Path (Path, Abs, Rel, Dir, File)
import qualified Path
import qualified Path.IO      as PathIO
import qualified Network.HTTP.Req

import Control.Concurrent (threadDelay)
import Data.Conduit

downloads :: Path Rel Dir
downloads = $(Path.mkRelDir "downloads")

testfile :: Path Rel File
testfile = downloads Path.</> $(Path.mkRelFile "thatignorefile")


getResource :: T.Text -> IO T.Text
getResource url = do res <- downloadOrGetFile url
                     TIO.readFile $ Path.toFilePath res

downloadOrGetFile :: T.Text -> IO (Path Abs File)
downloadOrGetFile url = do let name = last $ T.splitOn "/" url
                           res <- getFile name
                           case res of
                             Just file -> pure file
                             Nothing -> do downloadFile url name
                                           threadDelay 1000000
                                           downloadOrGetFile url

downloadFile :: T.Text -> T.Text -> IO ()
downloadFile url name = return () -- runConduit $ sourceUrl url .| sinkFile (Path.)


getFile :: T.Text -> IO (Maybe (Path Abs File))
getFile filename = do PathIO.ensureDir downloads :: IO ()
                      (_,files) <- PathIO.listDir downloads
                      return $ safeHead $ filter (matchfilename filename) files

matchfilename :: T.Text -> Path b File -> Bool
matchfilename filename file = T.unpack filename == Path.toFilePath (Path.filename file)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead []    = Nothing
