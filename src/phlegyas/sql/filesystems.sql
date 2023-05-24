-- :name create-filesystems-table
-- :command :execute
-- :result :raw
-- :doc Create filesystems table
create table filesystems (
       uuid blob primary key,
       name text not null unique,
       bsize integer not null,
       rnode integer not null,
       foreign key (rnode) references nodes (path)
);

-- :name insert-filesystem :i!
-- :doc Inserts filesystem into database
insert into filesystems (uuid, name, bsize, rnode) values (:uuid, :name, :bsize, :rnode);

-- :name get-filesystem :?
-- :result :1
-- :doc Get filesystem by name
select uuid, name, bsize, rnode from filesystems where name = :name;
