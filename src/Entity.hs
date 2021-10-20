{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Entity
    ( Chat (Chat)
    , Membership (Membership)
    , Message (Message)
    , User (User)
    ) where

import Data.Int
import Data.Aeson
import Data.Swagger
import Data.Text
import Data.Time
import Database.SQLite.Simple
import GHC.Generics (Generic)

data User = User
    { _user_id       :: Int64
    , _user_username :: Text
    , _user_password :: Text
    , _user_created  :: UTCTime
    , _user_archived :: Maybe UTCTime
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data Chat = Chat
    { _chat_id       :: Int64
    , _chat_name     :: Text
    , _chat_created  :: UTCTime
    , _chat_archived :: Maybe UTCTime
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data Membership = Membership
    { _membership_user_id :: Int64
    , _membership_chat_id :: Int64
    , _membership_joined  :: UTCTime
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data Message = Message
    { _message_id      :: Int64
    , _message_user_id :: Int64
    , _message_chat_id :: Int64
    , _message_content :: Text
    , _message_sent    :: UTCTime
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field
instance ToRow User where
    toRow (User a b c d e) = toRow (a, b, c, d, e)

instance FromRow Chat where
    fromRow = Chat <$> field <*> field <*> field <*> field
instance ToRow Chat where
    toRow (Chat a b c d) = toRow (a, b, c, d)

instance FromRow Membership where
    fromRow = Membership <$> field <*> field <*> field
instance ToRow Membership where
    toRow (Membership a b c) = toRow (a, b, c)

instance FromRow Message where
    fromRow = Message <$> field <*> field <*> field <*> field <*> field
instance ToRow Message where
    toRow (Message a b c d e) = toRow (a, b, c, d, e)
