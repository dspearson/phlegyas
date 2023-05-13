-- :name create-blocks-table
-- :command :execute
-- :result :raw
-- :doc Create blocks table
create table blocks (
    uuid blob primary key,
    node blob not null,
    idx integer not null,
    data blob,
    foreign key (uuid) references nodes (uuid)
);
