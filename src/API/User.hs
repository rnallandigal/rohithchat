{-# LANGUAGE OverloadedStrings   #-}

module API.User
    ( userServer
    ) where

import           Control.Concurrent.MVar
import           Control.Monad.Catch hiding (Handler)
import qualified Control.Monad.Catch as E (Handler (..))
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.Text
import qualified Database.SQLite.Simple as DB
import           Debug.Trace (trace)
import qualified Network.WebSockets as WS
import           Servant

import           API.Spec
import           API.Utils
import           Common
import           API.Membership (postMembership)
import           Entity
import           Model
import           Protocol
import qualified Query as Q

userServer :: GlobalState -> Server UserAPI
userServer gs = getUser gs :<|> postUser gs

getUser :: GlobalState -> UserQuery -> Handler [User]
getUser (GlobalState dbPool _) = liftIO . withResource dbPool . Q.user

postUser :: GlobalState -> UserReq -> Handler User
postUser g@(GlobalState dbPool clientConns) req@(UserReq u _) =
    handleAll (quit err500) $ do
        (user, userID) <- liftIO $ withResource dbPool $ \conn -> do
            Q.insertUser req conn
            userID <- DB.lastInsertRowId conn
            res <- Q.user (UserQuery (Just userID) Nothing Nothing) conn
            return $ (Prelude.head res, userID)
        postMembership g (MembershipReq userID 1)
        return user
