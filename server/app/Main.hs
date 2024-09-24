{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CompilerOpts
import Database
import Server

import Data.Text as T
import Database.SQLite.Simple as DSQS
import Network.HTTP.Client as NHTTPC
import Network.HTTP.Client.TLS
import Network.Wai.Handler.Warp
import System.Environment as SE

main :: IO ()
main = do
  --Get command line arguments.
  (_,files) <- SE.getArgs >>= compilerOpts
  DSQS.withConnection ":memory:" $ \conn -> do
    -- Set up HTTP prerequisite.
    manager <- newManager tlsManagerSettings
    execute_ conn imagetablecreation
    execute_ conn imageobjectdetectiontablecreation
    -- Set up environment.
    let imageserverenv = ImageServerEnv
                           { imageserverenv_sqliteconn      = conn
                           , imageserverenv_httpmanager     = manager
                           , imageserverenv_imaggaapikey    = T.pack $ files !! 0
                           , imageserverenv_imaggaapisecret = T.pack $ files !! 1
                           }
    run 8080 (imageApp imageserverenv)
