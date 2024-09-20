{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.SQLite.Simple

databasename :: String
databasename = "image.db"

imagetablecreation :: String
imagetablecreation = "CREATE TABLE IF NOT EXISTS image (image_identifier INTEGER PRIMARY KEY, image_label TEXT, image_filepath TEXT)"

imageobjectdetectiontablecreation :: String
imageobjectdetectiontablecreation = "CREATE TABLE IF NOT EXISTS image_object_detection (image_object_detection_id INTEGER, object TEXT, FOREIGN KEY(image_object_detection_id) REFERENCES image(image_identifier))"

getallimagemetadataquerystring :: Query
getallimagemetadataquerystring = "SELECT * FROM image"

getimagesquerystring :: Query
getimagesquerystring = "SELECT image.image_identifer, image,image_label, image.image_filepath FROM image INNER_JOIN image_object_detection ON image.image_identifier=image_object_detection.image_object_detection_id WHERE image_object_detection.object == :object"

getimagebyidquerystring :: Query
getimagebyidquerystring = "SELECT * FROM image WHERE image_identifier == :id"

postimagequerystring :: Query
postimagequerystring = "INSERT INTO image (image_identifier, image_label, image_filepath) VALUES (?,?,?)"
