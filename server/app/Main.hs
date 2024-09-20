{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Main (main) where

import CompilerOpts
import Database
import Server

import Data.Function as DF
import Data.List as DL
import Data.Text as T
import Database.SQLite.Simple as DSQS
import Network.HTTP.Client as NHTTPC
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Wai.Handler.Warp
--import Network.HTTP.Client.TLS as NHTTPT
--import Network.HTTP.Req
--import Network.HTTP.Types as NHTTPT
import System.Environment as SE

main :: IO ()
main = do
  --Get command line arguments.
  (_,files) <- SE.getArgs >>= compilerOpts
  -- Create sqlite database to store image records.
  --conn <- open databasename--"image.db"
  --execute_ conn imagetablecreation--"CREATE TABLE IF NOT EXISTS image (image_identifier INTEGER PRIMARY KEY, image_label TEXT, image_filepath TEXT)"
  --execute_ conn imageobjectdetectiontablecreation--"CREATE TABLE IF NOT EXISTS image_object_detection (image_object_detection_id INTEGER, object TEXT, FOREIGN KEY(image_object_detection_id) REFERENCES image(image_identifier))"
  DSQS.withConnection "" $ \conn -> do
    -- Set up HTTP prerequisite.
    manager <- newManager defaultManagerSettings
    execute_ conn imagetablecreation--"CREATE TABLE IF NOT EXISTS image (image_identifier INTEGER PRIMARY KEY, image_label TEXT, image_filepath TEXT)"
    execute_ conn imageobjectdetectiontablecreation--"CREATE TABLE IF NOT EXISTS image_object_detection (image_object_detection_id INTEGER, object TEXT, FOREIGN KEY(image_object_detection_id) REFERENCES image(image_identifier))"
    -- Set up environment.
    let imageserverenv = ImageServerEnv
                           { imageserverenv_sqliteconn      = conn
                           , imageserverenv_httpmanager     = manager
                           , imageserverenv_imaggaapikey    = T.pack $ files !! 0
                           , imageserverenv_imaggaapisecret = T.pack $ files !! 1
                           }
    run 8080 (imageApp imageserverenv)
  --close conn
