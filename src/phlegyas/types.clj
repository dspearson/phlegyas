(ns phlegyas.types
  (:require [clojure.set :refer :all]
            [phlegyas.reader :refer :all]
            [phlegyas.util :refer :all]
            [primitive-math :as math
             :refer [int->uint short->ushort
                     uint->int ushort->short
                     ubyte->byte byte->ubyte]]))

; function lookup namespaces
(def reader "phlegyas.reader/reader-")

; protocol defaults
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

(def role-access {:oread  #{:read       }
                  :owrite #{:write      }
                  :ordrw  #{:read :write}
                  :oexec  #{:execute    }
                  :otrunc #{:write      }})

(def permission-mode {:ixoth 0001
                      :iwoth 0002
                      :iroth 0004
                      :ixgrp 0010
                      :iwgrp 0020
                      :irgrp 0040
                      :ixusr 0100
                      :iwusr 0200
                      :irusr 0400})

(def java-permission-mode {:OWNER_EXECUTE  0100
                           :OWNER_WRITE    0200
                           :OWNER_READ     0400
                           :GROUP_EXECUTE  0010
                           :GROUP_WRITE    0020
                           :GROUP_READ     0040
                           :OTHERS_EXECUTE 0001
                           :OTHERS_WRITE   0002
                           :OTHERS_READ    0004})

(def file-mode {:dmdir    0x80000000
                :dmappend 0x40000000
                :dmexcl   0x20000000
                :dmtmp    0x04000000
                :dmauth   0x08000000})

(def qt-mode {:qtfile   (byte    0)
              :qtdir    (byte -128)
              :qtappend (byte   64)
              :qtexcl   (byte   32)
              :qttmp    (byte    4)
              :qtauth   (byte    8)})

(def message-type {:Tversion 100
                   :Rversion 101
                   :Tauth    102
                   :Rauth    103
                   :Tattach  104
                   :Rattach  105
                   :Rerror   107
                   :Tflush   108
                   :Rflush   109
                   :Twalk    110
                   :Rwalk    111
                   :Topen    112
                   :Ropen    113
                   :Tcreate  114
                   :Rcreate  115
                   :Tread    116
                   :Rread    117
                   :Twrite   118
                   :Rwrite   119
                   :Tclunk   120
                   :Rclunk   121
                   :Tremove  122
                   :Rremove  123
                   :Tstat    124
                   :Rstat    125
                   :Twstat   126
                   :Rwstat   127})

(def frame-layouts {:Tversion  [:tag :msize :version]
                    :Rversion  [:tag :msize :version]

                    :Tauth     [:tag :afid :uname :aname]
                    :Rauth     [:tag :aqid]

                    :Rerror    [:tag :ename]

                    :Tflush    [:tag :oldtag]
                    :Rflush    [:tag]

                    :Tattach   [:tag :fid :afid :uname :aname]
                    :Rattach   [:tag :qtype :qvers :qpath]

                    :Twalk     [:tag :fid :newfid :wname]
                    :Rwalk     [:tag :nwqids]

                    :Topen     [:tag :fid :iomode]
                    :Ropen     [:tag :qtype :qvers :qpath :iounit]

                    :Tcreate   [:tag :fid :name :perm :iomode]
                    :Rcreate   [:tag :qtype :qvers :qpath :iounit]

                    :Tread     [:tag :fid :offset :count]
                    :Rread     [:tag :data]

                    :Twrite    [:tag :fid :offset :data]
                    :Rwrite    [:tag :count]

                    :Tclunk    [:tag :fid]
                    :Rclunk    [:tag]

                    :Tremove   [:tag :fid]
                    :Rremove   [:tag]

                    :Tstat     [:tag :fid]
                    :Rstat     [:tag :ssize :size :type :dev :qtype :qvers :qpath :mode :atime :mtime :len :name :uid :gid :muid]

                    :Twstat    [:tag :fid :ssize :size :type :dev :qtype :qvers :qpath :mode :atime :mtime :len :name :uid :gid :muid]
                    :Rwstat    [:tag]})

(def type-size {:tag    2
                :oldtag 2
                :msize  4
                :size   2
                :ssize  2
                :unixfd 4
                :fid    4
                :afid   4
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
                 :oldtag  #((memfn putShort x) %1 %2)   ;     oldtag[2]
                 :msize   #((memfn   putInt x) %1 %2)   ;      msize[4]
                 :version #((memfn      put x) %1 %2)   ;    version[s]
                 :ename   #((memfn      put x) %1 %2)   ;      ename[s]
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
                 :data    #((memfn      put x) %1 %2)   ; count*data[1]
                 :iomode  #((memfn      put x) %1 %2)   ;       mode[1]
                 :count   #((memfn   putInt x) %1 %2)   ;      count[4]

                 ;; stat[n]
                 :type    #((memfn putShort x) %1 %2)   ;       type[2]
                 :dev     #((memfn   putInt x) %1 %2)   ;        dev[4]
                 :qtype   #((memfn      put x) %1 %2)   ;   qid.type[1]
                 :qvers   #((memfn   putInt x) %1 %2)   ;   qid.vers[4]
                 :qpath   #((memfn  putLong x) %1 %2)   ;   qid.path[8]
                 :name    #((memfn      put x) %1 %2)   ;       name[s]
                 :mode    #((memfn   putInt x) %1 %2)   ;       mode[4]
                 :atime   #((memfn   putInt x) %1 %2)   ;      atime[4]
                 :mtime   #((memfn   putInt x) %1 %2)   ;      mtime[4]
                 :len     #((memfn  putLong x) %1 %2)   ;     length[8]
                 :size    #((memfn putShort x) %1 %2)   ;       size[2]
                 :ssize   #((memfn putShort x) %1 %2)   ;       size[2]
                 :uid     #((memfn      put x) %1 %2)   ;        uid[s]
                 :gid     #((memfn      put x) %1 %2)   ;        gid[s]
                 :muid    #((memfn      put x) %1 %2)}) ;       muid[s]

(def type-resolvers          ((fn [] (into {} (for [[k v] type-bufop] [k (-> (str reader (name k)) symbol resolve)])))))
(def reverse-message-type    (gen-lookup message-type))
(def reverse-access-mode     (gen-lookup access-mode))
(def reverse-permission-mode (gen-lookup permission-mode))
(def reverse-file-mode       (gen-lookup file-mode))
(def reverse-qt-mode         (gen-lookup qt-mode))
