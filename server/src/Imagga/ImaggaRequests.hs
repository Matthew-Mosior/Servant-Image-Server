{-# LANGUAGE OverloadedStrings          #-}

module Imagga.ImaggaRequests where

import Tags as T

import Control.Monad.Reader
import Data.Aeson hiding (encode)
import Data.ByteString as DB (concat)
import Data.ByteString.Base64 hiding (decode)
import Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAccept,hAuthorization,hContentType)

{-
imaggaRequestUpload :: Manager -> FilePath -> Text -> Text -> IO Text
imaggaRequestUpload httpmanager imagefilepath apikey apisecret = do  
  imaggauploadrequest  <- parseRequest $ "https://api.imagga.com/v2/uploads?image=" ++
                                         imagefilepath
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
  _ <- putStrLn $ show imaggauploadid
  return imaggauploadid
-}

imaggaRequestTag :: Manager -> String -> Text -> Text -> IO [Text]
imaggaRequestTag httpmanager imageurl apikey apisecret = do
  imaggatagrequest <- parseRequest $ "https://api.imagga.com/v2/tags?image_url=" ++
                                     imageurl
  let imaggaauthorizationheader = encode $ encodeUtf8 $ T.pack $ (T.unpack apikey) ++ ":" ++ (T.unpack apisecret)
  let imaggaauthorizationheader' = DB.concat [ "Basic "
                                             , imaggaauthorizationheader
                                             ]
  let imaggatagrequest' = imaggatagrequest
                               { method         = "GET"
                               , requestHeaders = [ ( hAuthorization
                                                    , imaggaauthorizationheader' 
                                                    )
                                                  , ( hAccept
                                                    , "application/json"
                                                    )
                                                  , ( hContentType
                                                    , "application/json"
                                                    )
                                                  ]
                               }
  imaggatagresponse <- liftIO $ httpLbs imaggatagrequest'
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
