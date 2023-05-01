-- :name create-nodes-table
-- :command :execute
-- :result :raw
-- :doc Create nodes table
create table nodes (
    id integer primary key,
    type integer not null,
    name text not null,
    handler text
);
