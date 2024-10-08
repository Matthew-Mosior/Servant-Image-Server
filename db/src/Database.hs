{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.SQLite.Simple

databasename :: String
databasename = ":memory:"

imagetablecreation :: Query
imagetablecreation = "CREATE TABLE IF NOT EXISTS image (image_identifier INTEGER PRIMARY KEY, image_label TEXT, image_url TEXT)"

imageobjectdetectiontablecreation :: Query
imageobjectdetectiontablecreation = "CREATE TABLE IF NOT EXISTS image_object_detection (image_object_detection_id INTEGER, object TEXT, FOREIGN KEY(image_object_detection_id) REFERENCES image(image_identifier))"

getallimagemetadataquerystring :: Query
getallimagemetadataquerystring = "SELECT * FROM image"

getimagesquerystring :: Query
getimagesquerystring = "SELECT image.image_identifier, image.image_label, image.image_url FROM image INNER JOIN image_object_detection ON image.image_identifier=image_object_detection.image_object_detection_id WHERE image_object_detection.object == :object"

getimagebyidquerystring :: Query
getimagebyidquerystring = "SELECT * FROM image WHERE image_identifier == :id"

postimageinsertimagequerystring :: Query
postimageinsertimagequerystring = "INSERT INTO image (image_identifier, image_label, image_url) VALUES (?,?,?)"

postimageinsertobjectsquerystring :: Query
postimageinsertobjectsquerystring = "INSERT INTO image_object_detection (image_object_detection_id, object) VALUES (?,?)"
