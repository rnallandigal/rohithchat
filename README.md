rohithchat
------------

Installation (Source):
1) Install haskell stack: https://docs.haskellstack.org/en/stable/README/
2) Clone git repo
3) stack build
4) Run with `stack exec main -- --port 8080`

Data Model (Relational)
    - Users
    - Chats
    - User <-> Chat
    - ChatMessages

APIs
    - HTTP REST (Manage system state)
        - User CRUD
        - Chat CRUD
            - User Add/Remove
        - ChatMessage CRUD
    - WebSockets (Retrieve current state -> Update UI)
        - User stats (online / total)
        - ChatMessage (chat, user joined / left)

MVP
    - Define state and message types
    - Dynamically create database tables
    - Database connection pooling
    - Manage client connections using MVars
    - Backend API logic
        - HTTP REST
        - WebSockets
    - Frontend client logic
        - HTTP REST
        - WebSockets
    - UI display logic

Features
    - Secure communications and authentication
        - REST
            - REST over HTTPS (TLS)
            - JWT / cookie based authentication / authorization
        - WebSockets
            - Secure websockets
            - Reconnect / reestablish dropped connections
    - Pin chat messages
    - Functional testing
    - Performance testing
    - Read receipts
    - [DONE] Pagination
    - Race conditions in returning last inserted record (Transactions)
    - Error and exception handling
    - [DONE] Dynamic SQL query construction (constraints)
    - Relax uniqueness constraints on user and chat names
    - Range queries (Ex. all messages sent after '2021-10-16')
    - Rewrite UI display logic using Elm
    - Server side caching of database requests
