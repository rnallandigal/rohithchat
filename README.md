rohithchat
===========

Installation (Source):
-----------------------
1. Install haskell stack: https://docs.haskellstack.org/en/stable/README/
2. Clone git repo
3. stack build
4. Run with `stack exec main -- --port 8080`

Data Model (Relational)
------------------------
- User (id)
- Chat (id)
- Membership (user_id, chat_id)
- Message (id)

APIs
-----
- HTTP (Manage system state)
    - User CRUD
    - Chat CRUD
    - Membership CRUD
    - Message CRUD
- WebSockets (Send/Receive current state -> Update UI)
    - User stats (online / total)
    - ChatMessage (chat, user joined / left)
    - MembershipUpdate (user joined chat)

MVP
----
- Define state and message types
- Dynamically create database tables
- Database connection pooling
- Manage client connections using MVars
- Backend API logic
    - HTTP
    - WebSockets
- Frontend client logic
    - HTTP
    - WebSockets
- UI display logic

Features
---------
- [ ] Secure communications and authentication
    - HTTP
        - HTTPS (TLS)
        - JWT / cookie based authentication / authorization
    - WebSockets
        - Secure websockets
        - Reconnect / reestablish dropped connections
- [ ] Pin chat messages
- [ ] Functional testing
- [ ] Performance testing
- [ ] Read receipts
- [x] Pagination
- [ ] Race conditions in returning last inserted record (Transactions)
- [ ] Error and exception handling
- [x] Dynamic SQL query construction (constraints)
- [ ] Relax uniqueness constraints on user and chat names
- [ ] Range queries (Ex. all messages sent after '2021-10-16')
- [ ] Rewrite UI display logic using Elm
- [ ] Server side caching of database requests
