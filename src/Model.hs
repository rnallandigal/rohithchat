{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model
    ( ChatQuery (ChatQuery)
    , ChatReq (ChatReq)
    , MembershipQuery (MembershipQuery)
    , MembershipReq (MembershipReq)
    , MessageQuery (MessageQuery)
    , MessageReq (MessageReq)
    , Pagination (Pagination)
    , UserQuery (UserQuery)
    , UserReq (UserReq)
    ) where

import Data.Aeson
import Data.Int (Int64)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)

data Pagination = Pagination
    { _pagination_size :: Maybe Int
    , _pagination_num  :: Maybe Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data UserQuery = UserQuery
    { _user_query_id         :: Maybe Int64
    , _user_query_username   :: Maybe Text
    , _user_query_pagination :: Maybe Pagination
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data UserReq = UserReq
    { _user_req_username :: Text
    , _user_req_password :: Maybe Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ChatQuery = ChatQuery
    { _chat_query_id         :: Maybe Int64
    , _chat_query_name       :: Maybe Text
    , _chat_query_pagination :: Maybe Pagination
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ChatReq = ChatReq
    { _chat_req_chatname :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MembershipQuery = MembershipQuery
    { _membership_query_user       :: Maybe Int64
    , _membership_query_chat       :: Maybe Int64
    , _membership_query_pagination :: Maybe Pagination
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MembershipReq = MembershipReq
    { _membership_req_user_id :: Int64
    , _membership_req_chat_id :: Int64
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MessageQuery = MessageQuery
    { _message_query_id         :: Maybe Int64
    , _message_query_user       :: Maybe Int64
    , _message_query_chat       :: Maybe Int64
    , _message_query_pagination :: Maybe Pagination
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data MessageReq = MessageReq
    { _message_req_user_id :: Int64
    , _message_req_chat_id :: Int64
    , _message_req_content :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
