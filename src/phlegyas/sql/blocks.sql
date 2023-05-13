-- :name create-blocks-table
-- :command :execute
-- :result :raw
-- :doc Create blocks table
create table blocks (
    uuid blob primary key,
    node_id blob not null,
    block_index integer not null,
    data blob not null,
    foreign key (node_id) references nodes (uuid)
);
