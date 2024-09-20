{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CompilerOpts
import Server

import Data.List
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Req

main :: IO ()
main = do
  --Get command line arguments.
  (_,files) <- SE.getArgs >>= compilerOpts
  -- Create sqlite database to store image records.
  conn <- open databasename--"image.db"
  execute_ conn imagetablecreation--"CREATE TABLE IF NOT EXISTS image (image_identifier INTEGER PRIMARY KEY, image_label TEXT, image_filepath TEXT)"
  execute_ conn imageobjectdetectiontablecreation--"CREATE TABLE IF NOT EXISTS image_object_detection (image_object_detection_id INTEGER, object TEXT, FOREIGN KEY(image_object_detection_id) REFERENCES image(image_identifier))"
  -- Set up HTTP prerequisites.
  mymanager <- NHTTPC.newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
  let httpconfig = HttpConfig { httpConfigProxy = Nothing
                              , httpConfigRedirectCount = 10
                              , httpConfigAltManager = Just mymanager
                              , httpConfigCheckResponse = \_ response preview ->
                                  let scode = statusCodeR response
                                    in if | 200 <= scode && scode < 300
                                          -> Nothing
                                          | otherwise
                                          -> Just $ StatusCodeException (void response)
                                                                        preview
                              , httpConfigRetryPolicy = retryPolicyDefault
                              , httpConfigRetryJudge  = \_ response ->
                                  statusCodeR response
                                    `DL.elem` ([ 408 -- Request timeout
                                              , 504 -- Gateway timeout
                                              , 524 -- A timeout occured
                                              , 598 -- (Informal convention) Network read timeout error
                                              , 599 -- (Informal convention) Network connect timeout error
                                              ] :: [Int])
                              , httpConfigRetryJudgeException = \_ e ->
                                  case fromException e of
                                    Just (HttpExceptionRequest _ c) ->
                                      case c of
                                        ResponseTimeout -> True
                                        ConnectionTimeout -> True
                                        _ -> False
                                    _ -> False
                              , httpConfigBodyPreviewLength = 1024
                              }
                                where
                                  statusCodeR = NHTTPT.statusCode DF.. responseStatus
  -- Set up environment.
  let imageserverenv = ImageServerEnv
                         { imageserverenv_sqliteconn      = conn
			 , imageserverenv_httpmanager     = mymanager
			 , imageserverenv_httpconfig      = httpconfig
                         , imageserverenv_imaggaapikey    = files !! 0
			 , imageserverenv_imaggaapisecret = files !! 1
                         }
  run 8080 imageApp
  close conn
