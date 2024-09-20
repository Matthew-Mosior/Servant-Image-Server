{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Server

main :: IO ()
main = do
  -- Create sqlite database to store image records.
  conn <- open "image.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS image (image_identifier INTEGER PRIMARY KEY, image_label TEXT, image_filepath TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS image_object_detection (image_object_detection_id INTEGER, object TEXT, FOREIGN KEY(image_object_detection_id) REFERENCES image(image_identifier))"
  run 8080 imageApp
  close conn
