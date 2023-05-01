-- :name create-blocks-table
-- :command :execute
-- :result :raw
-- :doc Create blocks table
create table blocks (
    id integer primary key,
    node_id integer not null,
    block_index integer not null,
    data blob not null,
    primary key (node_id, block_index),
    foreign key (node_id) references nodes (id)
);
