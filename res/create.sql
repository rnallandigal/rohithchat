create table if not exists users (
    id          integer primary key autoincrement,
    username    varchar unique not null,
    password    varchar not null,
    created     datetime not null,
    archived    datetime
);

create table if not exists chats (
  id        integer primary key autoincrement,
  name      varchar unique not null,
  created   datetime not null,
  archived  datetime
);
insert or ignore into chats (name, created) values ('General', current_timestamp);

create table if not exists memberships (
  user_id   integer references users(id),
  chat_id   integer references chats(id),
  joined    datetime not null,
  primary key (user_id, chat_id)
);

create table if not exists messages (
  id        integer primary key autoincrement,
  user_id   integer references users(id),
  chat_id   integer references chats(id),
  content   varchar not null,
  sent      datetime not null,
  foreign key (user_id, chat_id) references memberships (user_id, chat_id)
);
