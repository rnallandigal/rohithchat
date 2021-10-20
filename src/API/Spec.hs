{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API.Spec where

import Data.Int (Int64)
import Data.Swagger
import Data.Text
import Servant
import Servant.API.WebSocket

import Entity
import Model

type API            = HttpAPI :<|> WebSocketAPI :<|> SwaggerAPI :<|> Raw

type HttpAPI        = "api" :> "v1" :> ( UserAPI
                                  :<|>   ChatAPI
                                  :<|>   MembershipAPI
                                  :<|>   MessageAPI    )

type WebSocketAPI   = "ws"  :> "v1" :> WebSocket

type SwaggerAPI     = "swagger.json" :> Get '[JSON] Swagger
                 :<|> "swagger"      :> Raw

type UserAPI        = "user" :> "search"
                             :> ReqBody '[JSON] UserQuery :> Post '[JSON] [User]
                 :<|> "user" :> ReqBody '[JSON] UserReq :> Post '[JSON] User

type ChatAPI        = "chat" :> "search"
                             :> ReqBody '[JSON] ChatQuery :> Post '[JSON] [Chat]
                 :<|> "chat" :> ReqBody '[JSON] ChatReq :> Post '[JSON] Chat

type MembershipAPI  = "membership" :> "search"
                                   :> ReqBody '[JSON] MembershipQuery
                                   :> Post '[JSON] [Membership]
                 :<|> "membership" :> ReqBody '[JSON] MembershipReq
                                   :> Post '[JSON] ()
--                 :<|> "membership" :> ReqBody '[JSON] MembershipReq
--                                   :> Delete '[JSON] ()

type MessageAPI     = "message" :> "search"
                                :> ReqBody '[JSON] MessageQuery
                                :> Post '[JSON] [Message]
                 :<|> "message" :> ReqBody '[JSON] MessageReq
                                :> Post '[JSON] ()
