{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import API.API
import CompilerOpts
import Database
import Server

import Control.Monad.IO.Unlift
import Control.Monad.Except
import Control.Monad.Reader
import Database.SQLite.Simple as DSQS
import Data.Text as T
import Network.HTTP.Client as NHTTPC
import Network.HTTP.Client.TLS
import System.Environment as SE
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  --Get command line arguments.
  (_,files) <- SE.getArgs >>= compilerOpts
  liftIO $ DSQS.withConnection ":memory:" $ \conn -> do
    -- Set up HTTP prerequisite.
    manager <- newManager tlsManagerSettings
    execute_ conn imagetablecreation
    execute_ conn imageobjectdetectiontablecreation
    let imageserverenv = ImageServerEnv
                           { imageserverenv_sqliteconn      = conn
                           , imageserverenv_httpmanager     = manager
                           , imageserverenv_imaggaapikey    = T.pack $ files !! 0
                           , imageserverenv_imaggaapisecret = T.pack $ files !! 1
                           }      
    testWithApplication (pure $ imageApp imageserverenv) $ \_ ->
      withArgs [] $ hspec $ do
        describe "POST /images" $ do
          it "uploads a new image of a cat" $ do
            let newImage  = ImageInput "https://www.thesprucepets.com/thmb/RnGTM5ENlWu-D4oND397jksaUCw=/1911x0/filters:no_upscale():strip_icc()/bombay-cat-full-profile-history-and-care-5202250-hero-85810f454cf84a7786e650136a10f91c.jpg" (Just "poppie") True
            post' <- runExceptT $ runReaderT (runImageServerT $ postImage newImage) imageserverenv
            case post' of
              Left err     ->
                putStrLn $ show err
              Right post'' ->
                (imageinput'_url post'') `shouldBe` "https://www.thesprucepets.com/thmb/RnGTM5ENlWu-D4oND397jksaUCw=/1911x0/filters:no_upscale():strip_icc()/bombay-cat-full-profile-history-and-care-5202250-hero-85810f454cf84a7786e650136a10f91c.jpg"
        describe "GET /images" $ do
          it "can grab an image by its identifier" $ do
            let newImage  = ImageInput "https://docs.imagga.com/static/images/docs/sample/japan-605234_1280.jpg" (Just "japan") True
            post' <- runExceptT $ runReaderT (runImageServerT $ postImage newImage) imageserverenv
            case post' of
              Left err     ->
                putStrLn $ show err
              Right post'' -> do
                getjapanimage <- runExceptT $ runReaderT (runImageServerT $ getImageById (imageinput'_identifier post'')) imageserverenv
                case getjapanimage of
                  Left err          ->
                    putStrLn $ show err
                  Right japanimage' ->
                    japanimage' `shouldBe` [ Image
                                               { image_identifer = image_identifer $ Prelude.head japanimage'
                                               , image_label     = image_label $ Prelude.head japanimage'
                                               , image_url       = image_url $ Prelude.head japanimage'
                                               }
                                           ]
        describe "GET /images" $ do
          it "can grab an image by its label" $ do
            post' <- runExceptT $ runReaderT (runImageServerT $ getImages (Just "kitty")) imageserverenv
            case post' of
              Left err     ->
                putStrLn $ show err
              Right post'' -> do
                Prelude.length post'' `shouldBe` 1
        describe "GET /images" $ do
         it "can grab all images metadata" $ do
           post' <- runExceptT $ runReaderT (runImageServerT $ getImages Nothing) imageserverenv
           case post' of
             Left err     ->
               putStrLn $ show err
             Right post'' -> do
               Prelude.length post'' `shouldNotBe` 1
