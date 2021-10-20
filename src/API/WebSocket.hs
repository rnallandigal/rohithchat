{-# LANGUAGE OverloadedStrings #-}

module API.WebSocket
    ( wsServer
    ) where

import           Debug.Trace (trace)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Catch hiding (Handler)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Control.Monad.Catch as E (Handler(..))
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.Text
import           Data.Time
import qualified Database.SQLite.Simple as DB
import qualified Network.WebSockets as WS
import           Servant

import           API.Spec
import           API.Message (postMessage)
import           Common
import           Entity
import           Model
import           Protocol
import qualified Query as Q


wsServer :: GlobalState -> Server WebSocketAPI
wsServer g@(GlobalState _ clientConns) conn = liftIO $ do
    WS.withPingThread conn 10 (pure ()) $ do
        userID <- authenticate conn
        modifyMVar_ clientConns $ return . M.insert userID conn
        trace (show $ encode $ SubmitMessage 3 "blah") $ return ()
        forever $ do
            opt <- try $ WS.receiveData conn
            case opt of
                Left e@(WS.CloseRequest _ _) -> trace (show e) $ throwM e
                Left e@(WS.ConnectionClosed) -> trace (show e) $ throwM e
                Left e -> trace (show e) $ return ()
                Right msg -> handleMsg g userID $ decode msg

authenticate :: WS.Connection -> IO Int64
authenticate conn = do
    opt <- try $ WS.receiveData conn
    case opt of
        Left e@(WS.CloseRequest _ _) -> trace (show e) $ throwM e
        Left e@(WS.ConnectionClosed) -> trace (show e) $ throwM e
        Left e -> trace (show e) $ authenticate conn
        Right msg -> trace ("Got message: " <> show msg) $ case decode msg of
            Just (Authenticate i) -> return i
            _                     -> authenticate conn

handleMsg :: GlobalState -> Int64 -> Maybe ClientMessage -> IO ()
handleMsg g u (Just (SubmitMessage c m)) = postMessage g (MessageReq u c m)
handleMsg g u msg = trace (show msg) $ return ()
