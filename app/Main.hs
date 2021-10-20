{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch hiding (Handler)
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.String
import           Data.Text
import           Database.SQLite.Simple as DB
import           Database.SQLite3 as DB (exec)
import qualified Network.WebSockets as WS

import           Command
import           Common
import           Model
import           Server
import qualified Query as Q

main :: IO ()
main = readCommand >>= doCommand

doCommand :: Command -> IO ()
doCommand (Run port) = do
    dbPool <- createPool createConn DB.close 1 30 10
    clientConns <- newMVar M.empty
    let gs = (GlobalState dbPool clientConns)
    resetDB gs
    start port gs

createConn :: IO Connection
createConn = do
    conn <- open ".run/messaging.db"
    execute_ conn "pragma foreign_keys=on"
    setTrace conn (Just $ putStrLn . unpack)
    return conn

resetDB :: GlobalState -> IO ()
resetDB (GlobalState dbPool _) = do
    dropSQL <- readFile "res/drop.sql"
    createSQL <- readFile "res/create.sql"
    liftIO $ withResource dbPool $ \conn -> do
--        DB.exec (connectionHandle conn) $ pack dropSQL
        DB.exec (connectionHandle conn) $ pack createSQL
