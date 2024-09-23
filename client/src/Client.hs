module Client where

import API.API

import Data.Proxy
import Servant.Client

imageServerClient :: Client ClientM ImageServerAPI
imageServerClient = client (Proxy :: Proxy ImageServerAPI)
