{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import API     (imageServerAPI)
import API.API (ImageInput(..),ImageInput'(..),Image(..),ImageObjectDetection(..),ImageServerAPI)
import Database
import Imagga.ImaggaRequests

import Control.Monad (forM,forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString as DB
import Data.ByteString.Base64 as B64
import Database.SQLite.Simple
import Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Servant.API
import Servant.Server
import Text.Regex (mkRegex, matchRegex)

-- Environment 
data ImageServerEnv = ImageServerEnv
  { imageserverenv_sqliteconn      :: Connection
  , imageserverenv_httpmanager     :: Manager
  , imageserverenv_imaggaapikey    :: Text
  , imageserverenv_imaggaapisecret :: Text
  } 

newtype ImageServerT a = ImageServerT 
 { runImageServerT :: ReaderT ImageServerEnv (ExceptT ServerError IO) a
 } deriving (Functor, Applicative, Monad, MonadIO, MonadReader ImageServerEnv, MonadThrow, MonadError ServerError)

readerToHandler :: ImageServerEnv -> ImageServerT a -> Handler a
readerToHandler env imageservert = do
  val <- liftIO $ runExceptT $ runReaderT (runImageServerT imageservert) env
  case val of
    Left e  -> throwError e
    Right s -> return s

-- GET `/images`
-- Returns HTTP `200` OK with a JSON response containing all image metadata.
-- GET `/images?objects="dog,cat"`
-- Returns a HTTP `200` OK with a JSON response body containing only images
-- that have the detected objects specified in the query parameter.
getImages :: Maybe Text -> ImageServerT [Image]
getImages querystring = do
  env <- ask
  case querystring of
    Nothing            -> do
      results <- liftIO $
                   query_ (imageserverenv_sqliteconn env)
                          getallimagemetadataquerystring :: ImageServerT [Image]
      case results of
        []       ->
          throwError $
            err500 { errBody = "No data to return from sqlite3 backend (empty database)."
                   }
        results' ->
          return results'
    Just querystring'' -> do
      -- Validate format of querystring.
      let querystringregex = mkRegex "^[^,]+(,[^,]+)*$"
        in case matchRegex querystringregex (T.unpack querystring'') of
         Nothing ->
           throwError $
             err500 { errBody = "Could not parse querystring."
                    }
         Just _  -> do
           let querystring' = splitOn "," querystring''
           results <-
             forM querystring' $ \currentquerystring ->
               liftIO $ queryNamed (imageserverenv_sqliteconn env)
                                   getimagesquerystring
                                   [":object" := currentquerystring]
           case Prelude.concat results of
             []       ->
               throwError $
                 err500 { errBody = "No image data contains labels provided."
                        }
             results' ->
               return results'

-- GET `/images/{imageId}`
-- Returns HTTP `200` OK with a JSON response containing image metadata for the specified image.
getImageById :: Int -> ImageServerT [Image]
getImageById imageid = do
  env     <- ask
  result' <- liftIO $ queryNamed (imageserverenv_sqliteconn env)
                                 getimagebyidquerystring
                                 [":id" := imageid]
  case result' of
    []       ->
      throwError $
        err500 { errBody = "No image data at identifier."
               }
    result'' ->
      return result''

-- POST `/images`
-- Send a JSON request body including an image file or URL, an optional label for the image,
-- and an optional field to enable object detection.
--
-- Returns a HTTP `200` OK with a JSON response body including the image data,
-- its label (generate one if the user did not provide it),
-- its identifier provided by the persistent data store,
-- and any objects detected (if object detection was enabled).
postImage :: ImageInput -> ImageServerT ImageInput'
postImage imageinput = do
  env <- ask
  case imageinput_enableobjectdetection imageinput of
    False -> do
      imagedatarequest <- liftIO $ parseRequest (T.unpack $ imageinput_url imageinput)
      imagedataresponse <- liftIO $ httpLbs imagedatarequest (imageserverenv_httpmanager env)
      let imagedata'' = B64.encode $ toStrict $ responseBody imagedataresponse
      let status      = responseStatus imagedataresponse
      rowId <- liftIO $ lastInsertRowId $ imageserverenv_sqliteconn env
      let imageinput_label' = case imageinput_label imageinput of
                                Nothing    -> T.pack $ show rowId
                                Just label -> label
      _ <- liftIO $ execute (imageserverenv_sqliteconn env)
                            postimageinsertimagequerystring
                            ( Image (fromIntegral rowId)
                                    imageinput_label'
                                    ( imageinput_url imageinput
                                    )
                            )
      return $ ImageInput'
                 { imageinput'_url                   = imageinput_url imageinput
                 , imageinput'_data                  = decodeUtf8 imagedata''
                 , imageinput'_label                 = imageinput_label' 
                 , imageinput'_enableobjectdetection = False
                 , imageinput'_identifier            = fromIntegral rowId
                 , imageinput'_responsestatuscode    = statusCode status 
                 }
    True  -> do
      (imaggatags,status) <- liftIO $ imaggaRequestTag (imageserverenv_httpmanager env)
                                                       (T.unpack $ imageinput_url imageinput)
                                                       (imageserverenv_imaggaapikey env)
                                                       (imageserverenv_imaggaapisecret env)
      imagedatarequest <- liftIO $ parseRequest (T.unpack $ imageinput_url imageinput)
      imagedataresponse <- liftIO $ httpLbs imagedatarequest (imageserverenv_httpmanager env)
      let imagedata'' = B64.encode $ toStrict $ responseBody imagedataresponse
      rowId <- liftIO $ lastInsertRowId $ imageserverenv_sqliteconn env
      let imageinput_label' = case imageinput_label imageinput of
                                Nothing    -> T.pack $ show rowId
                                Just label -> label
      _ <- liftIO $ execute (imageserverenv_sqliteconn env)
                            postimageinsertimagequerystring
                            ( Image (fromIntegral rowId)
                                    imageinput_label'
                                    ( imageinput_url imageinput
                                    )
                            )
      _ <- liftIO $ forM_ imaggatags $ \currenttag ->
             execute (imageserverenv_sqliteconn env)
                     postimageinsertobjectsquerystring
                     ( ImageObjectDetection (fromIntegral rowId)
                                            currenttag
                     )
      return $ ImageInput'
                 { imageinput'_url                   = imageinput_url imageinput
                 , imageinput'_data                  = decodeUtf8 imagedata''
                 , imageinput'_label                 = imageinput_label' 
                 , imageinput'_enableobjectdetection = True
                 , imageinput'_identifier            = fromIntegral rowId
                 , imageinput'_responsestatuscode    = status
                 }

imageServerT :: ServerT ImageServerAPI ImageServerT
imageServerT = getImages           :<|>
               getImageById        :<|>
               postImage

imageServer :: ImageServerEnv -> Server ImageServerAPI
imageServer env = hoistServer imageServerAPI (readerToHandler env) imageServerT

imageApp :: ImageServerEnv -> Application
imageApp env = serveWithContext imageServerAPI EmptyContext (imageServer env)
