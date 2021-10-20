{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Protocol where

import Data.Aeson
import Data.Int (Int64)
import Data.Swagger
import Data.Text (Text)
import GHC.Generics (Generic)

import Entity
import Model

data Update = Added | Removed
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ClientMessage
    = Authenticate Int64
    | SubmitMessage Int64 Text
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data ServerMessage
    = PushMessage Message
    | MembershipUpdate Int64 Int64
    deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
