{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.API (ImageServerAPI)

import Data.Proxy

imageServerAPI :: Proxy ImageServerAPI
imageServerAPI = Proxy
