{-# LANGUAGE OverloadedStrings #-}

module API.Membership
    ( membershipServer
    , postMembership
    ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Pool
import qualified Data.Set as S
import           Data.Text
import           Database.SQLite.Simple as DB
import           Debug.Trace (trace)
import qualified Network.WebSockets as WS
import           Servant

import           API.Spec
import           API.Utils
import           Common
import           Entity
import           Model
import           Protocol
import qualified Query as Q

membershipServer :: GlobalState -> Server MembershipAPI
membershipServer gs = getMembership gs
                 :<|> postMembership gs
--                 :<|> deleteMembership gs

getMembership :: GlobalState -> MembershipQuery -> Handler [Membership]
getMembership (GlobalState dbPool _) = liftIO . withResource dbPool . Q.membership

postMembership :: GlobalState -> MembershipReq -> Handler ()
postMembership (GlobalState dbPool clientConns) req@(MembershipReq u c) = do
    userIDs <- liftIO $ withResource dbPool $ \conn -> do
        Q.insertMembership req conn
        getUsersByChat conn c
    liftIO $ broadcast clientConns (const $ MembershipUpdate u c) userIDs

deleteMembership :: GlobalState -> MembershipReq -> Handler ()
deleteMembership = undefined
