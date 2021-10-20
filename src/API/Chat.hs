{-# LANGUAGE OverloadedStrings #-}

module API.Chat
    ( chatServer
    ) where

import           Control.Monad.Catch (handleAll)
import           Control.Monad.IO.Class
import           Data.Pool
import qualified Database.SQLite.Simple as DB
import           Servant

import           API.Spec
import           API.Utils
import           Common
import           Entity
import           Model
import qualified Query as Q

chatServer :: GlobalState -> Server ChatAPI
chatServer gs = getChat gs :<|> postChat gs

getChat :: GlobalState -> ChatQuery -> Handler [Chat]
getChat (GlobalState dbPool _) = liftIO . withResource dbPool . Q.chat

postChat :: GlobalState -> ChatReq -> Handler Chat
postChat (GlobalState dbPool _) req@(ChatReq c) =
    handleAll (quit err500) $ liftIO $ withResource dbPool $ \conn -> do
        Q.insertChat req conn
        rowid <- DB.lastInsertRowId conn
        res <- Q.chat (ChatQuery (Just rowid) Nothing Nothing) conn
        return $ Prelude.head res
