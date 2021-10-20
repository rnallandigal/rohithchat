{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
    ( start
    ) where

import Network.Wai.Handler.Warp
import Servant

import API.Spec
import API.Swagger
import API.User
import API.Chat
import API.Membership
import API.Message
import API.Utils
import API.WebSocket
import Common

start :: Int -> GlobalState -> IO ()
start port gs = run port $ app gs

app :: GlobalState -> Application
app gs = serve (Proxy :: Proxy API) $ server gs

server :: GlobalState -> Server API
server gs = httpServer gs
       :<|> wsServer gs
       :<|> swaggerServer
       :<|> staticServer "site/"

httpServer :: GlobalState -> Server HttpAPI
httpServer gs = userServer gs
           :<|> chatServer gs
           :<|> membershipServer gs
           :<|> messageServer gs
