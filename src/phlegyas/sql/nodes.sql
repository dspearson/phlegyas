-- :name create-nodes-table
-- :command :execute
-- :result :raw
-- :doc Create nodes table
create table nodes (
    parent blob not null,
    handler text not null,
    type integer not null,
    vers integer not null,
    path blob primary key,
    mode integer not null,
    atime integer not null,
    mtime integer not null,
    length integer not null, -- 0 for directories, as they are computed on the fly
    name text not null,
    uid text not null,
    gid text not null,
    muid text not null,
    unique(parent, name)
);

-- :name insert-node :i!
-- :doc Inserts node
insert into nodes (parent,
                   handler,
                   type,
                   vers,
                   path,
                   mode,
                   atime,
                   mtime,
                   length,
                   name,
                   uid,
                   gid,
                   muid)
values (:parent,
        :handler,
        :qid-type,
        :qid-vers,
        :qid-path,
        :mode,
        :atime,
        :mtime,
        :length,
        :name,
        :uid,
        :gid,
        :muid);

-- :name get-node :?
-- :result :1
-- :doc Get node by uuid
select * from nodes where path = :qid-path;

-- :name get-children :?
-- :result :raw
-- :doc Get children nodes by uuid
select * from nodes where parent = :qid-path and path != :qid-path;
