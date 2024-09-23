{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Imagga.ImaggaRequests where

import API     (imageServerAPI)
import API.API (ImageInput(..),ImageInput'(..),Image(..),ImageServerAPI)
import Database
import Tags as T
import Uploads as U

import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy as DBL
import Database.SQLite.Simple
import Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import Network.HTTP.Client
import Servant.API
import Servant.Server

imaggaRequestUpload :: Manager -> FilePath -> IO Text
imaggaRequestUpload httpmanager imageinput_filepath = do
  imaggauploadrequest  <- parseRequest $ "https://api.imagga.com/v2/uploads?image=" ++
                                         imageinput_filepath
  let imaggauploadrequest' = imaggauploadrequest
                               { method = "POST"
                               }
  imaggauploadresponse <- liftIO $ httpLbs imaggauploadrequest'
                                           httpmanager
  let imaggauploadresponse' = decode $ responseBody imaggauploadresponse :: Maybe UploadResponse
  let imaggauploadid        = case imaggauploadresponse' of
                                Nothing                  ->
                                  T.empty
                                Just imaggauploadresponse'' ->
                                  uploadId $ U.result imaggauploadresponse''
  return imaggauploadid

imaggaRequestTag :: Manager -> String -> IO [Text]
imaggaRequestTag httpmanager uploadid = do
  imaggatagrequest  <- parseRequest $ "https://api.imagga.com/v2/tags?image_upload_id=" ++
                                      uploadid
  imaggatagresponse <- liftIO $ httpLbs imaggatagrequest
                                        httpmanager
  let imaggatagresponse' = decode $ responseBody imaggatagresponse :: Maybe TagResponse
  let imaggatags         = case imaggatagresponse' of
                             Nothing                  ->
                               []
                             Just imaggatagresponse'' ->
                               Prelude.map fst                    $
                               Prelude.filter (\(_,y) -> y >= 50) $
                               Prelude.map (\x -> ((en . tag) x, confidence x))
                                           (tags $ T.result imaggatagresponse'')
  return imaggatags
