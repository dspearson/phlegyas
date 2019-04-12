(ns phlegyas.types
  (:require [clojure.set :refer :all]
            [phlegyas.buffers :refer :all]
            [phlegyas.util :refer :all]
            [primitive-math :as math
             :refer [ubyte->byte
                     uint->int
                     ushort->short
                     ulong->long]]))

; for dynamic lookup and resolution of buffer reader functions.
(def buffer-function-prefix "phlegyas.buffers/get-")

; protocol defaults.
(def protocol-version   "9P2000")
(def notag                0xffff)
(def nofid            0xffffffff)
(def stat-keep-number 0xffffffff)
(def max-message-size 0x7fffffff)
(def stat-keep-string         "")

(def frame-byte {:Tversion 100
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
                    :Rauth     [:tag :qid-type :qid-vers :qid-path]

                    :Rerror    [:tag :ename]

                    :Tflush    [:tag :oldtag]
                    :Rflush    [:tag]

                    :Tattach   [:tag :fid :afid :uname :aname]
                    :Rattach   [:tag :qid-type :qid-vers :qid-path]

                    :Twalk     [:tag :fid :newfid :wnames]
                    :Rwalk     [:tag :nwqids]

                    :Topen     [:tag :fid :iomode]
                    :Ropen     [:tag :qid-type :qid-vers :qid-path :iounit]

                    :Tcreate   [:tag :fid :name :perm :iomode]
                    :Rcreate   [:tag :qid-type :qid-vers :qid-path :iounit]

                    :Tread     [:tag :fid :offset :count]
                    :Rread     [:tag :data]

                    :Twrite    [:tag :fid :offset :data]
                    :Rwrite    [:tag :count]

                    :Tclunk    [:tag :fid]
                    :Rclunk    [:tag]

                    :Tremove   [:tag :fid]
                    :Rremove   [:tag]

                    :Tstat     [:tag :fid]
                    :Rstat     [:tag :ssize :size :type :dev :qid-type :qid-vers :qid-path :mode :atime :mtime :length :name :uid :gid :muid]

                    :Twstat    [:tag :fid :ssize :size :type :dev :qid-type :qid-vers :qid-path :mode :atime :mtime :length :name :uid :gid :muid]
                    :Rwstat    [:tag]})

(def type-size {:tag       2
                :oldtag    2
                :msize     4
                :size      2
                :ssize     2
                :fid       4
                :afid      4
                :newfid    4
                :perm      4
                :iounit    4
                :offset    8
                :type      2
                :dev       4
                :qid-type  1
                :qid-vers  4
                :qid-path  8
                :iomode    1
                :mode      4
                :atime     4
                :mtime     4
                :length    8
                :count     4})

(def buffer-operator {:tag      #((memfn ^java.nio.ByteBuffer putShort ^Short   x) %1 (ushort->short %2)) ;         tag[2]
                      :oldtag   #((memfn ^java.nio.ByteBuffer putShort ^Short   x) %1 (ushort->short %2)) ;      oldtag[2]
                      :msize    #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;       msize[4]
                      :fid      #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;         fid[4]
                      :afid     #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;        afid[4]
                      :newfid   #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;      newfid[4]
                      :perm     #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;        perm[4]
                      :iounit   #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;      iounit[4]
                      :offset   #((memfn ^java.nio.ByteBuffer  putLong ^Long    x) %1 (ulong->long   %2)) ;      offset[8]
                      :iomode   #((memfn ^java.nio.ByteBuffer      put ^Byte    x) %1 (ubyte->byte   %2)) ;        mode[1]
                      :count    #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;       count[4]

                      ;; stat[n]
                      :type     #((memfn ^java.nio.ByteBuffer putShort ^Short   x) %1 (ushort->short %2)) ;        type[2]
                      :dev      #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;         dev[4]
                      :qid-type #((memfn ^java.nio.ByteBuffer      put ^Byte    x) %1 (ubyte->byte   %2)) ;    qid.type[1]
                      :qid-vers #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;    qid.vers[4]
                      :qid-path #((memfn ^java.nio.ByteBuffer  putLong ^Long    x) %1 (ulong->long   %2)) ;    qid.path[8]
                      :name     #((memfn ^java.nio.ByteBuffer      put ^Byte    x) %1 (ubyte->byte   %2)) ;        name[s]
                      :mode     #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;        mode[4]
                      :atime    #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;       atime[4]
                      :mtime    #((memfn ^java.nio.ByteBuffer   putInt ^Integer x) %1 (uint->int     %2)) ;       mtime[4]
                      :length   #((memfn ^java.nio.ByteBuffer  putLong ^Long    x) %1 (ulong->long   %2)) ;      length[8]
                      :size     #((memfn ^java.nio.ByteBuffer putShort ^Short   x) %1 (ushort->short %2)) ;        size[2]
                      :ssize    #((memfn ^java.nio.ByteBuffer putShort ^Short   x) %1 (ushort->short %2)) ;        size[2]

                      ;; these fields have transformers
                      :version  nil                                                                       ;     version[s]
                      :ename    nil                                                                       ;       ename[s]
                      :uname    nil                                                                       ;       uname[s]
                      :aname    nil                                                                       ;       aname[s]
                      :wnames   nil                                                                       ; count*wname[s]
                      :data     nil                                                                       ;  count*data[n]
                      :nwqids   nil                                                                       ;  nwqid*qid[13]
                      :uid      nil                                                                       ;         uid[s]
                      :gid      nil                                                                       ;         gid[s]
                      :muid     nil})                                                                     ;        muid[s]

;; we iterate over the keys in the buffer-operator map, and resolve functions for reading them.
(def buffer-functions ((fn [] (into {} (for [[k v] buffer-operator] [k (-> (str buffer-function-prefix (name k)) symbol resolve)])))))
(def reverse-frame-byte (reverse-map frame-byte))
