-- :name create-nodes-table
-- :command :execute
-- :result :raw
-- :doc Create nodes table
create table nodes (
    uuid blob primary key,
    type integer not null,
    name text not null,
    parent blob not null,
    handler text not null
);

-- :name insert-node :i!
-- :doc Inserts node
insert into nodes (uuid, type, name, parent, handler) values (:uuid, :type, :name, :parent, :handler);

-- :name get-node :?
-- :result :1
-- :doc Get node by uuid
select uuid, type, name, parent, handler from nodes where uuid = :uuid;
