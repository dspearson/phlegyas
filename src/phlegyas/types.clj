(ns phlegyas.types
  (:require [clojure.set :refer :all]
            [phlegyas.reader :refer :all]
            [phlegyas.util :refer :all]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte]]))

; function lookup namespaces
(def reader       "phlegyas.reader/reader-")

; protocol constants
(def protocol-version   "9P2000")
(def notag                0xffff)
(def nofid            0xffffffff)
(def stat-keep-number 0xffffffff)
(def max-message-size 0x7fffffff)
(def stat-keep-string         "")

(def access-mode {:oread  0x00
                  :owrite 0x01
                  :ordrw  0x02
                  :oexec  0x03
                  :otrunc 0x10
                  :rclose 0x40
                  :oexcl  0x1000})

(def role-access {:oread  #{:read}
                  :owrite #{:write}
                  :ordrw  #{:read :write}
                  :oexec  #{:execute}
                  :otrunc #{:write}})

(def permission-mode {:ixoth 0001
                      :iwoth 0002
                      :iroth 0004
                      :ixgrp 0010
                      :iwgrp 0020
                      :irgrp 0040
                      :ixusr 0100
                      :iwusr 0200
                      :irusr 0400})

(def java-permission-mode {:OWNER_EXECUTE 0100
                           :OWNER_WRITE 0200
                           :OWNER_READ 0400
                           :GROUP_EXECUTE 0010
                           :GROUP_WRITE 0020
                           :GROUP_READ 0040
                           :OTHERS_EXECUTE 0001
                           :OTHERS_WRITE 0002
                           :OTHERS_READ 0004})

(def file-mode {:dmdir    0x80000000
                :dmappend 0x40000000
                :dmexcl   0x20000000
                :dmtmp    0x04000000
                :dmauth   0x08000000})

(def qt-mode {:qtfile   (byte (ubyte->byte 0x0))
              :qtdir    (byte (ubyte->byte 0x80))
              :qtappend (byte (ubyte->byte 0x40))
              :qtexcl   (byte (ubyte->byte 0x20))
              :qttmp    (byte (ubyte->byte 0x4))
              :qtauth   (byte (ubyte->byte 0x8))})

(def message-type {:version 100
                   :auth    102
                   :attach  104
                   :error   106
                   :flush   108
                   :walk    110
                   :open    112
                   :create  114
                   :read    116
                   :write   118
                   :clunk   120
                   :remove  122
                   :stat    124
                   :wstat   126})

(def Tframe {:clunk   '((:tag) (:fid))
             :remove  '((:tag) (:fid))
             :stat    '((:tag) (:fid))
             :open    '((:tag) (:fid) (:iomode))
             :openfd  '((:tag) (:fid) (:mode))
             :version `((:tag) (:msize ~max-message-size) (:version ~protocol-version))
             :auth    '((:tag) (:afid) (:uname) (:aname))
             :read    '((:tag) (:fid) (:offset) (:count))
             :create  '((:tag) (:fid) (:name) (:perm) (:mode))
             :attach  `((:tag) (:fid) (:afid ~nofid) (:uname) (:aname))
             :write   '((:tag) (:fid) (:offset) (:count) (:data))
             :walk    '((:tag) (:fid) (:newfid) (:wname))
             :wstat   '((:tag) (:fid) (:size) (:ssize) (:type) (:dev) (:qtype) (:qvers)
                        (:qpath) (:mode) (:atime) (:mtime) (:len) (:name) (:uid) (:gid) (:muid))})

(def Rframe {:wstat   '((:tag))
             :clunk   '((:tag))
             :remove  '((:tag))
             :auth    '((:tag) (:aqid))
             :attach  '((:tag) (:qtype) (:qvers) (:qpath))
             :walk    '((:tag) (:nwqid) (:nwqids))
             :write   '((:tag) (:count))
             :error   '((:tag) (:ename))
             :flush   '((:tag) (:oldtag))
             :open    '((:tag) (:qtype) (:qvers) (:qpath) (:iounit))
             :create  '((:tag) (:qid) (:iounit))
             :read    '((:tag) (:data))
             :version `((:tag) (:msize) (:version ~protocol-version))
             :openfd  '((:tag) (:qid) (:iounit) (:unixfd))
             :stat    '((:tag) (:size) (:ssize) (:type) (:dev) (:qtype) (:qvers)
                        (:qpath) (:mode) (:atime) (:mtime) (:len) (:name) (:uid) (:gid) (:muid))})

(def type-size {:tag    2
                :msize  4
                :size   2
                :ssize  2
                :unixfd 4
                :fid    4
                :newfid 4
                :nwname 2
                :nwqid  2
                :perm   4
                :iounit 4
                :offset 8
                :aqid  13
                :qid   13
                :type   2
                :dev    4
                :qtype  1
                :qvers  4
                :qpath  8
                :iomode 1
                :mode   4
                :atime  4
                :mtime  4
                :len    8
                :count  4})

(def type-bufop {:tag     #((memfn putShort x) %1 %2)   ;        tag[2]
                 :msize   #((memfn   putInt x) %1 %2)   ;      msize[4]
                 :version #((memfn      put x) %1 %2)   ;    version[s]
                 :ename   #((memfn      put x) %1 %2)   ;      ename[s]
                 :unixfd  #((memfn   putInt x) %1 %2)   ;     unixfd[4]
                 :uname   #((memfn      put x) %1 %2)   ;      uname[s]
                 :aname   #((memfn      put x) %1 %2)   ;      aname[s]
                 :fid     #((memfn   putInt x) %1 %2)   ;        fid[4]
                 :afid    #((memfn   putInt x) %1 %2)   ;       afid[4]
                 :newfid  #((memfn   putInt x) %1 %2)   ;       nfid[4]
                 :nwname  #((memfn putShort x) %1 %2)   ;     nwname[2]
                 :wname   #((memfn      put x) %1 %2)   ;      wname[s]
                 :perm    #((memfn   putInt x) %1 %2)   ;       perm[4]
                 :iounit  #((memfn   putInt x) %1 %2)   ;     iounit[4]
                 :offset  #((memfn  putLong x) %1 %2)   ;     offset[8]
                 :aqid    #((memfn      put x) %1 %2)   ;       qid[13]
                 :nwqid   #((memfn putShort x) %1 %2)   ;      nwqid[2]
                 :nwqids  #((memfn      put x) %1 %2)   ; nwqid*qid[13]

                 ;; stat[n]
                 :type    #((memfn putShort x) %1 %2)   ;       type[2]
                 :dev     #((memfn   putInt x) %1 %2)   ;        dev[4]
                 :qtype   #((memfn      put x) %1 %2)   ;   qid.type[1]
                 :qvers   #((memfn   putInt x) %1 %2)   ;   qid.vers[4]
                 :qpath   #((memfn  putLong x) %1 %2)   ;   qid.path[8]
                 :name    #((memfn      put x) %1 %2)   ;       name[s]
                 :mode    #((memfn   putInt x) %1 %2)   ;       mode[4]
                 :iomode  #((memfn      put x) %1 %2)   ;       mode[1]
                 :atime   #((memfn   putInt x) %1 %2)   ;      atime[4]
                 :mtime   #((memfn   putInt x) %1 %2)   ;      mtime[4]
                 :len     #((memfn  putLong x) %1 %2)   ;     length[8]
                 :size    #((memfn putShort x) %1 %2)   ;       size[2]
                 :ssize   #((memfn putShort x) %1 %2)   ;       size[2]
                 :count   #((memfn   putInt x) %1 %2)   ;      count[4]
                 :uid     #((memfn      put x) %1 %2)   ;        uid[s]
                 :gid     #((memfn      put x) %1 %2)   ;        gid[s]
                 :muid    #((memfn      put x) %1 %2)}) ;       muid[s]

; inverse lookup tables
(def type-resolvers ((fn [] (into {} (for [[k v] type-bufop] [k (-> (str reader (name k)) symbol resolve)])))))
(def message-type-r (gen-lookup message-type))
(def access-mode-r (gen-lookup access-mode))
(def permission-mode-r (gen-lookup permission-mode))
(def file-mode-r (gen-lookup file-mode))
(def qt-mode-r (gen-lookup qt-mode))
