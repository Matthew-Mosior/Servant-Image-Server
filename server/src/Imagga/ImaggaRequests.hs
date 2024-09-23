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
import Data.Aeson hiding (encode)
import Data.ByteString as DB (concat)
import Data.ByteString.Base64 hiding (decode)
import Data.ByteString.Lazy as DBL
import Database.SQLite.Simple
import Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Servant.API
import Servant.Server

imaggaRequestUpload :: Manager -> FilePath -> Text -> Text -> IO Text
imaggaRequestUpload httpmanager imageinput_filepath apikey apisecret = do  
  imaggauploadrequest  <- parseRequest $ "https://api.imagga.com/v2/uploads?image=" ++
                                         imageinput_filepath
  let imaggaauthorizationheader = encode $ encodeUtf8 $ T.pack $ (T.unpack apikey) ++ ":" ++ (T.unpack apisecret)
  let imaggaauthorizationheader' = DB.concat [ "Basic "
                                             , imaggaauthorizationheader
                                             ]
  _ <- putStrLn $ show imaggaauthorizationheader'
  let imaggauploadrequest' = imaggauploadrequest
                               { method         = "POST"
                               , requestHeaders = [ ( hAuthorization
                                                    , imaggaauthorizationheader'
                                                    )
                                                  ]
                               }
  imaggauploadresponse <- liftIO $ httpLbs imaggauploadrequest'
                                           httpmanager
  _ <- putStrLn $ show imaggauploadresponse
  let imaggauploadresponse' = decode $ responseBody imaggauploadresponse :: Maybe UploadResponse
  let imaggauploadid        = case imaggauploadresponse' of
                                Nothing                  ->
                                  T.empty
                                Just imaggauploadresponse'' ->
                                  uploadId $ U.result imaggauploadresponse''
  return imaggauploadid

imaggaRequestTag :: Manager -> String -> Text -> Text -> IO [Text]
imaggaRequestTag httpmanager uploadid apikey apisecret = do
  imaggatagrequest  <- parseRequest $ "https://api.imagga.com/v2/tags?image_upload_id=" ++
                                      uploadid
  let imaggaauthorizationheader = encode $ encodeUtf8 $ T.pack $ (T.unpack apikey) ++ ":" ++ (T.unpack apisecret)
  let imaggaauthorizationheader' = DB.concat [ "Basic "
                                             , imaggaauthorizationheader
                                             ]
  let imaggatagrequest' = imaggatagrequest
                               { method         = "GET"
                               , requestHeaders = [ ( hAuthorization
                                                    , imaggaauthorizationheader' 
                                                    )
                                                  ]
                               }
  imaggatagresponse <- liftIO $ httpLbs imaggatagrequest'
                                        httpmanager
  _ <- putStrLn $ show imaggatagresponse
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
