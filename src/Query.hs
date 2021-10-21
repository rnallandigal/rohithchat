{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Query where

import Data.Int (Int64)
import Data.Maybe
import Data.Text (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ

import Entity
import Model
import QueryUtils

-- Simple queries
user :: UserQuery -> Connection -> IO [User]
user (UserQuery i u mp) conn =
    let cs = catMaybes [constraintQ "id" i, constraintQ "username" u]
        p = fromMaybe defPagination mp
    in  uncurry (queryNamed conn) $ selectQ "users" cs p ""

insertUser :: UserReq -> Connection -> IO ()
insertUser (UserReq u p) conn =
    execute conn [sql|
        insert into users (username, password, created)
                   values (?,?,current_timestamp)
    |] (u, fromMaybe "admin" p)

chat :: ChatQuery -> Connection -> IO [Chat]
chat (ChatQuery i c mp) conn =
    let cs = catMaybes [constraintQ "id" i, constraintQ "name" c]
        p = fromMaybe defPagination mp
    in  uncurry (queryNamed conn) $ selectQ "chats" cs p ""

insertChat :: ChatReq -> Connection -> IO ()
insertChat (ChatReq c) conn =
    execute conn [sql|
        insert into chats (name, created) values (?,current_timestamp)
    |] (Only c)

membership :: MembershipQuery -> Connection -> IO [Membership]
membership (MembershipQuery u c mp) conn =
    let cs = catMaybes [constraintQ "user_id" u, constraintQ "chat_id" c]
        p = fromMaybe defPagination mp
    in  uncurry (queryNamed conn) $ selectQ "memberships" cs p ""

allMemberships :: MembershipQuery -> Connection -> IO [Membership]
allMemberships (MembershipQuery u c _) conn =
    let cs = catMaybes [constraintQ "user_id" u, constraintQ "chat_id" c]
        p = Pagination (Just (-1)) Nothing
    in  uncurry (queryNamed conn) $ selectQ "memberships" cs p ""

insertMembership :: MembershipReq -> Connection -> IO ()
insertMembership (MembershipReq u c) conn =
    execute conn [sql|
        insert into memberships (user_id, chat_id, joined)
                   values (?,?,current_timestamp)
    |] (u, c)

deleteMembership :: MembershipReq -> Connection -> IO ()
deleteMembership (MembershipReq u c) conn =
    execute conn [sql|
        delete from memberships where user_id = ? and chat_id = ?
    |] (u, c)

message :: MessageQuery -> Connection -> IO [Message]
message (MessageQuery i u c mp) conn =
    let cs = catMaybes
                [ constraintQ "id" i
                , constraintQ "user_id" u
                , constraintQ "chat_id" c ]
        p = fromMaybe defPagination mp
    in  uncurry (queryNamed conn) $ selectQ "messages" cs p "order by sent desc"

insertMessage :: MessageReq -> Connection -> IO ()
insertMessage (MessageReq u c m) conn =
    execute conn [sql|
        insert into messages (user_id, chat_id, content, sent)
                   values (?,?,?,current_timestamp)
    |] (u, c, m)

-- Joins
chatsByUser :: Int64 -> Connection -> IO [Chat]
chatsByUser userID conn =
    query conn [sql|
        select * from chats where id in (
            select chat_id from memberships where user_id=?
        )
    |] (Only userID)

usersByChat :: Int64 -> Connection -> IO [User]
usersByChat chatID conn =
    query conn [sql|
        select * from users where id in (
            select user_id from memberships where chat_id=?
        )
    |] (Only chatID)
