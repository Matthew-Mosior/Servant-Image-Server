{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import GHC.Generics
import Data.Aeson as DA hiding (Result(..))
import Data.Text (Text)

-- Data type for the tag
data Tag = Tag
  { en :: Text
  } deriving (Show, Generic)

instance FromJSON Tag
instance ToJSON Tag

-- Data type for individual tag entry with confidence
data TagEntry = TagEntry
  { confidence :: Double
  , tag        :: Tag
  } deriving (Show, Generic)

instance FromJSON TagEntry
instance ToJSON TagEntry

-- Data type for result containing a list of tag entries
data Result = Result
  { tags :: [TagEntry]
  } deriving (Show, Generic)

instance FromJSON Result
instance ToJSON Result

-- Data type for status information
data Status = Status
  { text :: Text
  , type_ :: Text  -- 'type' is a reserved word in Haskell, so we use 'type_'
  } deriving (Show, Generic)

instance FromJSON Status where
  parseJSON = withObject "Status" $ \v -> Status
    <$> v .: "text"
    <*> v .: "type"

instance ToJSON Status where
  toJSON (Status txt typ) =
    object ["text" .= txt, "type" .= typ]

-- Main data type to capture the overall JSON structure
data ApiResponse = ApiResponse
  { result :: Result
  , status :: Status
  } deriving (Show, Generic)

instance FromJSON ApiResponse
instance ToJSON ApiResponse
