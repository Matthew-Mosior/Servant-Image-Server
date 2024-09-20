{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API.API where

import Database.SQLite.Simple
import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API

data ImageInput = ImageInput
  { imageinput_filepath              :: Text
  , imageinput_label                 :: Maybe Text
  , imageinput_enableobjectdetection :: Bool
  } deriving (Show, Generic)

instance FromJSON ImageInput
instance ToJSON ImageInput

data ImageInput' = ImageInput'
  { imageinput'_filepath              :: Text
  , imageinput'_data                  :: Text
  , imageinput'_label                 :: Text
  , imageinput'_enableobjectdetection :: Bool
  } deriving (Show, Generic)

instance FromJSON ImageInput'
instance ToJSON ImageInput'

data Image = Image
  { image_identifer  :: Int
  , image_label      :: Text
  , image_filepath   :: Text
  } deriving (Show, Generic)

data ImageObjectDetection = ImageObjectDetection
  { imageobjectdetection_identifier      :: Int
  , imageobjectdetection_objectsdetected :: [Text]
  } deriving (Show, Generic)

instance FromJSON Image
instance ToJSON Image

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field
instance ToRow Image where
  toRow (Image id_ label_ data_) = toRow (id_, label_, data_)

-- GET `/images`
-- Returns HTTP `200` OK with a JSON response containing all image metadata.
type ImageServerGetAllImageMetadata = "images" :> Get '[JSON] [Image]

-- GET `/images?objects="dog,cat"`
-- Returns a HTTP `200` OK with a JSON response body containing only images that have the detected objects specified in the query parameter.
type ImageServerGetImages           = "images" :> QueryParam "objects" Text :> Get '[JSON] [Image]

-- GET `/images/{imageId}`
-- Returns HTTP `200` OK with a JSON response containing image metadata for the specified image.
type ImageServerGetImageById        = "images" :> Capture "imageid" Int :> Get '[JSON] [Image]

-- POST `/images`
-- Send a JSON request body including an image file or URL, an optional label for the image,
-- and an optional field to enable object detection
type ImageServerPostImages          = "images" :> ReqBody '[JSON] ImageInput :> Post '[JSON] ImageInput'

type ImageServerAPI = ImageServerGetAllImageMetadata :<|>
                      ImageServerGetImages           :<|>
                      ImageServerGetImageById        :<|>
                      ImageServerPostImages 
