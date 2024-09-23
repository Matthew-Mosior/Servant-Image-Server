{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Uploads where

import Data.Text (Text)
import Data.Aeson as DA hiding (Result(..))
import GHC.Generics (Generic)

-- Define the relevant data types
data Result = Result
  { uploadId :: Text
  } deriving (Show, Generic)

-- Define FromJSON instances for parsing
instance FromJSON Result where
  parseJSON = withObject "Result" $ \v -> Result
    <$> v .: "upload_id"

instance ToJSON Result where
  toJSON (Result uploadId) =
    object ["upload_id" .= uploadId]

data Status = Status
  { text :: Text
  , statusType :: Text
  } deriving (Show, Generic)

instance FromJSON Status where
  parseJSON = withObject "Status" $ \v -> Status
    <$> v .: "text"
    <*> v .: "type"

instance ToJSON Status where
  toJSON (Status text statusType) =
    object ["text" .= text, "type" .= statusType]

data UploadResponse = UploadResponse
  { result :: Result
  , status :: Status
  } deriving (Show, Generic)

instance FromJSON UploadResponse where
  parseJSON = withObject "Response" $ \v -> UploadResponse
    <$> v .: "result"
    <*> v .: "status"

instance ToJSON UploadResponse where
  toJSON (UploadResponse result status) =
    object ["result" .= result, "status" .= status]
