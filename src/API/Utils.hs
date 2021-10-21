{-# LANGUAGE OverloadedStrings #-}

module API.Utils where

import           Control.Concurrent.MVar
import           Control.Monad.Catch hiding (Handler)
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Debug.Trace (trace)
import           Network.Wai.Application.Static
import qualified Network.WebSockets as WS
import qualified Database.SQLite.Simple as DB
import           Servant
import           WaiAppStatic.Types (unsafeToPiece)

import           Protocol
import           Model
import           Entity
import qualified Query as Q

staticServer :: String -> Server Raw
staticServer root = serveDirectoryWith $ (defaultWebAppSettings root)
    { ssIndices = Prelude.map unsafeToPiece ["index.html"]
    , ssRedirectToIndex = True
    , ssAddTrailingSlash = True }

quit :: (Exception e) => ServerError -> e -> Handler a
quit code e = throwError $ code { errBody = BLU.fromString $ show e }

unicast :: MVar (M.Map Int64 WS.Connection) -> ServerMessage -> Int64 -> IO ()
unicast mvar msg recipient = modifyMVar_ mvar $ \conns -> do
    case M.lookup recipient conns of
        Nothing -> trace ("User not found: " <> show recipient) $ return conns
        Just conn -> do
            res <- try $ WS.sendTextData conn $ encode msg
            case res of
                Left e@(WS.CloseRequest _ _)
                    -> trace (show e) $ return $ M.delete recipient conns
                Left e@(WS.ConnectionClosed)
                    -> trace (show e) $ return $ M.delete recipient conns
                Left e -> trace (show e) $ return conns
                Right () -> trace ("Sent message: " <> show msg) $ return conns

broadcast :: MVar (M.Map Int64 WS.Connection)
          -> (Int64 -> ServerMessage)
          -> [Int64]
          -> IO ()
broadcast mvar msgf = mapM_ $ (\u -> unicast mvar (msgf u) u)
