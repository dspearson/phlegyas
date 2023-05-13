-- :name create-filesystems-table
-- :command :execute
-- :result :raw
-- :doc Create filesystems table
create table filesystems (
       uuid blob primary key,
       name text not null,
       bsize integer not null,
       rnode integer not null,
       foreign key (rnode) references nodes (uuid)
);

-- :name insert-filesystem :i!
-- :doc Inserts filesystem into database
insert into filesystems (uuid, name, block_size, root_node) values (:uuid, :name, :block-size, :root-node);

-- :name get-filesystem :?
-- :result :1
-- :doc Get filesystem by name
select uuid, name, block_size, root_node from filesystems where name = :name;
