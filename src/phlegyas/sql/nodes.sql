-- :name create-nodes-table
-- :command :execute
-- :result :raw
-- :doc Create nodes table
create table nodes (
    uuid blob primary key,
    type integer not null,
    name text not null,
    parent_id blob not null,
    handler text
);

-- :name insert-node :i!
-- :doc Inserts node
insert into nodes (uuid, type, name, parent_id, handler) values (:uuid, :type, :name, :parent-id, :handler);
