{-# LANGUAGE OverloadedStrings #-}

module API.Message
    ( messageServer
    , postMessage
    ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.Text
import           Database.SQLite.Simple as DB
import qualified Network.WebSockets as WS
import           Servant

import           API.Spec
import API.Utils
import           Common
import           Entity
import           Model
import           Protocol
import qualified Query as Q

messageServer :: GlobalState -> Server MessageAPI
messageServer gs = getMessage gs :<|> postMessage gs

getMessage :: GlobalState -> MessageQuery -> Handler [Message]
getMessage (GlobalState dbPool _) = liftIO . withResource dbPool . Q.message

postMessage :: (MonadIO m) => GlobalState -> MessageReq -> m ()
postMessage (GlobalState dbPool clientConns) req@(MessageReq u c m) = liftIO $ do
    (msg, userIDs) <- withResource dbPool $ \conn -> do
        Q.insertMessage req conn
        rowid <- DB.lastInsertRowId conn
        let mq = MessageQuery (Just rowid) Nothing Nothing Nothing
        msg <- Prelude.head <$> Q.message mq conn
        userIDs <- getUsersByChat conn c
        return (msg, userIDs)
    liftIO $ broadcast clientConns (const $ PushMessage msg) userIDs
