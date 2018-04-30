{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module NetworkIO (getResource) where

import qualified Data.Text    as T
import qualified Data.ByteString.Lazy as BL

import           Path (Path, Abs, Rel, Dir, File)
import qualified Path
import qualified Path.IO      as PathIO

import           Network.HTTP.Simple
import           Conduit
import           Control.Concurrent (threadDelay)

downloads :: Path Rel Dir
downloads = $(Path.mkRelDir "downloads/")

getResource :: T.Text -> IO BL.ByteString
getResource url = do res <- downloadOrGetFile url
                     BL.readFile $ Path.toFilePath res


downloadOrGetFile :: T.Text -> IO (Path Abs File)
downloadOrGetFile url = do namepath <- filepath
                           res <- getFile namepath
                           case res of
                             Just file -> pure file
                             Nothing -> do downloadFile url namepath
                                           threadDelay 1000000
                                           downloadOrGetFile url
  where
    filepath = do let name = T.unpack $ last $ T.splitOn "/" url
                  namepath <- Path.parseRelFile name
                  pure $ downloads Path.</> namepath

downloadFile :: T.Text -> Path Rel File -> IO ()
downloadFile url name = do req <- parseRequest (T.unpack url)
                           runConduitRes $ httpSource req getResponseBody 
                                         .| sinkFile (Path.toFilePath name)


getFile :: Path Rel File -> IO (Maybe (Path Abs File))
getFile filename = do PathIO.ensureDir downloads :: IO ()
                      (_,files) <- PathIO.listDir downloads
                      return $ safeHead $ filter (matchfilename filename) files

matchfilename :: Path a File -> Path b File -> Bool
matchfilename filename file = Path.toFilePath (Path.filename filename) == Path.toFilePath (Path.filename file)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead []    = Nothing
