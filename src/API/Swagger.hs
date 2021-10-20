{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.Swagger
    ( swaggerServer
    ) where

import Control.Lens
import Data.Swagger
import Servant
import Servant.Swagger

import API.Spec
import API.Utils

swaggerServer :: Server SwaggerAPI
swaggerServer = return usersSwagger :<|> staticServer "site/swagger/"

usersSwagger :: Swagger
usersSwagger = toSwagger (Proxy :: Proxy HttpAPI)
  & info.title   .~ "Messaging API"
  & info.version .~ "0.1"
  & info.description ?~ "Interact with a chat message application"
  & info.license ?~ ("AllRightsReserved" & url ?~ URL "http://mit.com")
