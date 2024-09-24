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
  { imageinput_url                   :: Text
  , imageinput_label                 :: Maybe Text
  , imageinput_enableobjectdetection :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON ImageInput
instance ToJSON ImageInput

data ImageInput' = ImageInput'
  { imageinput'_url                   :: Text
  , imageinput'_data                  :: Text
  , imageinput'_label                 :: Text
  , imageinput'_enableobjectdetection :: Bool
  , imageinput'_identifier            :: Int
  , imageinput'_responsestatuscode    :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON ImageInput'
instance ToJSON ImageInput'

data Image = Image
  { image_identifer          :: Int
  , image_label              :: Text
  , image_url                :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Image
instance ToJSON Image

instance FromRow Image where
  fromRow = Image <$> field <*> field <*> field
instance ToRow Image where
  toRow (Image id_ label_ url_) = toRow (id_, label_, url_)

data ImageObjectDetection = ImageObjectDetection
  { imageobjectdetection_identifier     :: Int
  , imageobjectdetection_objectdetected :: Text
  } deriving (Show, Generic)

instance FromJSON ImageObjectDetection
instance ToJSON ImageObjectDetection

instance FromRow ImageObjectDetection where
  fromRow = ImageObjectDetection <$> field <*> field
instance ToRow ImageObjectDetection where
  toRow (ImageObjectDetection id_ object_) = toRow (id_, object_)

-- GET `/images`
-- Returns HTTP `200` OK with a JSON response containing all image metadata.
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

type ImageServerAPI = ImageServerGetImages    :<|>
                      ImageServerGetImageById :<|>
                      ImageServerPostImages
