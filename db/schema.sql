CREATE TABLE posts (
id serial primary key,
subject varchar(64),
date bigint,
content text,
tags text
);

CREATE TABLE users (
id serial primary key,
username varchar(64),
password text,
groups text
);
