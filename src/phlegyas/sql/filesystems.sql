-- :name create-filesystems-table
-- :command :execute
-- :result :raw
-- :doc Create filesystems table
create table filesystems (
       id integer primary key,
       name text not null,
       block_size integer not null
);
