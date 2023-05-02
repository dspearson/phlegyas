-- :name create-filesystems-table
-- :command :execute
-- :result :raw
-- :doc Create filesystems table
create table filesystems (
       id integer primary key,
       name text not null,
       block_size integer not null,
       root_node integer not null,
       foreign key (root_node) references nodes (id)
);

-- :name insert-filesystem :i!
-- :doc Inserts filesystem into database
insert into filesystems (name, block_size) values (:name, :block-size);

-- :name get-filesystem :?
-- :result :1
-- :doc Get filesystem by name

select name, block_size from filesystems inner join nodes
