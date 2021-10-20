module Common where

import           Control.Concurrent.MVar
import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Pool
import           Data.Text
import qualified Database.SQLite.Simple as DB
import qualified Network.WebSockets as WS
import           Servant

data GlobalState = GlobalState
    { _dbPool      :: Pool DB.Connection
    , _clientConns :: MVar (M.Map Int64 WS.Connection) }
