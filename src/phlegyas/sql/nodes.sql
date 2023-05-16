-- :name create-nodes-table
-- :command :execute
-- :result :raw
-- :doc Create nodes table
create table nodes (
    parent blob not null,
    handler text not null,
    qid_type integer not null,
    qid_vers integer not null,
    qid_path blob primary key,
    mode integer not null,
    atime integer not null,
    mtime integer not null,
    length integer not null, -- 0 for directories, as they are computed on the fly
    name text not null
    uid text not null,
    gid text not null,
    muid text not null,
    unique(parent, name)
);

-- :name insert-node :i!
-- :doc Inserts node
insert into nodes (parent,
                   handler,
                   qid_type,
                   qid_vers,
                   qid_path,
                   mode,
                   atime,
                   mtime,
                   length,
                   name,
                   uid,
                   gid,
                   muid)
values (:qid-path,
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
select * from nodes where qid_path = :qid-path;
