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
import Tags as T
import Uploads as U

import Control.Monad (forM,forM_)
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

imageServerT :: ServerT ImageServerAPI ImageServerT
imageServerT = getAllImageMetadata :<|>
               getImages           :<|>
               getImageById        :<|>
               postImage
  where
    -- GET `/images`
    -- Returns HTTP `200` OK with a JSON response containing all image metadata.
    getAllImageMetadata :: ImageServerT [Image]
    getAllImageMetadata = do
      env <- ask
      liftIO $ query_ (imageserverenv_sqliteconn env)
                      --"SELECT * FROM image" :: ImageServerT [Image]
                      getallimagemetadataquerystring :: ImageServerT [Image]                          

    -- GET `/images?objects="dog,cat"`
    -- Returns a HTTP `200` OK with a JSON response body containing only images
    -- that have the detected objects specified in the query parameter.
    getImages :: Maybe Text -> ImageServerT [Image]
    getImages querystring = do
      env <- ask
      querystring' <- case querystring of
                           Nothing            ->
                             throwError $ (ServerError 500 "Failed Computation" "" [])
                           Just querystring'' ->
                             return $ splitOn "," querystring'' 
      results <-
        forM querystring' $ \currentquerystring ->
          liftIO $ queryNamed (imageserverenv_sqliteconn env)
                              --"SELECT image.image_identifer, image,image_label, image.image_filepath FROM image INNER_JOIN image_object_detection ON image.image_identifier=image_object_detection.image_object_detection_id WHERE image_object_detection.object == :object"
                              getimagesquerystring
                              [":object" := currentquerystring]
      return $ Prelude.concat results
    
    -- GET `/images/{imageId}`
    -- Returns HTTP `200` OK with a JSON response containing image metadata for the specified image.
    getImageById :: Int -> ImageServerT [Image]
    getImageById imageid = do
      env    <- ask
      result <- liftIO $ queryNamed (imageserverenv_sqliteconn env)
                                    --"SELECT * FROM image WHERE image_identifier == :id"
                                    getimagebyidquerystring
                                    [":id" := imageid]
      return result
    
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
          imagedata' <- liftIO $ DBL.readFile $ T.unpack $ imageinput_filepath imageinput
          rowId <- liftIO $ lastInsertRowId $ imageserverenv_sqliteconn env
          let imageinput_label' = case imageinput_label imageinput of
                                    Nothing    -> T.pack $ show rowId
                                    Just label -> label
          _ <- liftIO $ execute (imageserverenv_sqliteconn env)
                                --"INSERT INTO image (image_identifier, image_label, image_filepath) VALUES (?,?,?)"
                                postimageinsertimagequerystring
                                ( Image (fromIntegral rowId)
                                        imageinput_label'
                                        ( imageinput_filepath imageinput
                                        )
                                )
          return $ ImageInput'
                     { imageinput'_filepath              = imageinput_filepath imageinput
                     , imageinput'_data                  = decodeUtf8 $ toStrict imagedata'
                     , imageinput'_label                 = imageinput_label' 
                     , imageinput'_enableobjectdetection = False 
                     }
        True  -> do
          {-
	  imaggauploadrequest  <- parseRequest $ "https://api.imagga.com/v2/uploads?image=" ++
                                                 (T.unpack $ imageinput_filepath imageinput)
	  let imaggauploadrequest' = imaggauploadrequest
                                       { method = "POST"
                                       }
          imaggauploadresponse <- liftIO $ httpLbs imaggauploadrequest'
                                                   (imageserverenv_httpmanager env)
          let imaggauploadresponse' = decode $ responseBody imaggauploadresponse :: Maybe UploadResponse
          let imaggauploadid        = case imaggauploadresponse' of
                                        Nothing                  ->
                                          T.empty
                                        Just imaggauploadresponse'' ->
                                          uploadId $ U.result imaggauploadresponse''
          -} 
          imaggauploadid <- liftIO $ imaggaRequestUpload (imageserverenv_httpmanager env)
                                                         (T.unpack $ imageinput_filepath imageinput)
          {-
          imaggatagrequest  <- parseRequest $ "https://api.imagga.com/v2/tags?image_upload_id=" ++
                                              (T.unpack imaggauploadid)
          imaggatagresponse <- liftIO $ httpLbs imaggatagrequest
                                                   (imageserverenv_httpmanager env)
          let imaggatagresponse' = decode $ responseBody imaggatagresponse :: Maybe TagResponse
          let imaggatags         = case imaggatagresponse' of
                                     Nothing                  ->
                                       []
                                     Just imaggatagresponse'' ->
                                       Prelude.map fst                    $
                                       Prelude.filter (\(_,y) -> y >= 50) $
                                       Prelude.map (\x -> ((en . tag) x, confidence x))
                                                   (tags $ T.result imaggatagresponse'')
          -}
          imaggatags <- liftIO $ imaggaRequestTag (imageserverenv_httpmanager env)
                                         (T.unpack imaggauploadid)
          imagedata' <- liftIO $ DBL.readFile $ T.unpack $ imageinput_filepath imageinput
          rowId <- liftIO $ lastInsertRowId $ imageserverenv_sqliteconn env
          let imageinput_label' = case imageinput_label imageinput of
                                    Nothing    -> T.pack $ show rowId
                                    Just label -> label
          _ <- liftIO $ execute (imageserverenv_sqliteconn env)
                                --"INSERT INTO image (image_identifier, image_label, image_filepath) VALUES (?,?,?)"
                                postimageinsertimagequerystring
                                ( Image (fromIntegral rowId)
                                        imageinput_label'
                                        ( imageinput_filepath imageinput
                                        )
                                )
          _ <- liftIO $ forM_ imaggatags $ \currenttag ->
                 execute (imageserverenv_sqliteconn env)
                         --"INSERT INTO image_object_detection (image_identifier, object) VALUES (?,?)"
                         postimageinsertobjectsquerystring
                         ( ImageObjectDetection (fromIntegral rowId)
                                                currenttag
                         )
          return $ ImageInput'
                     { imageinput'_filepath              = imageinput_filepath imageinput
                     , imageinput'_data                  = decodeUtf8 $ toStrict imagedata'
                     , imageinput'_label                 = imageinput_label' 
                     , imageinput'_enableobjectdetection = False 
                     }

imageServer :: ImageServerEnv -> Server ImageServerAPI
imageServer env = hoistServer imageServerAPI (readerToHandler env) imageServerT

imageApp :: ImageServerEnv -> Application
imageApp env = serveWithContext imageServerAPI EmptyContext (imageServer env)
