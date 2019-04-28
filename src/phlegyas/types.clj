(ns phlegyas.types
  (:require [clojure.set :refer :all]
            [phlegyas.util :refer :all]
            [phlegyas.buffers :refer :all]
            [primitive-math :as math
             :refer [ubyte->byte
                     uint->int
                     ushort->short
                     ulong->long]]))

(set! *warn-on-reflection* true)

; for dynamic lookup and resolution of buffer reader functions.
(def buffer-function-prefix "phlegyas.buffers/get-")

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

(def qt-mode {:file   (byte    0)
              :dir    (byte -128)
              :append (byte   64)
              :excl   (byte   32)
              :tmp    (byte    4)
              :auth   (byte    8)})

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

(def get-operation {:version  #'get-string
                    :name     #'get-string
                    :uname    #'get-string
                    :aname    #'get-string
                    :muid     #'get-string
                    :uid      #'get-string
                    :gid      #'get-string
                    :ename    #'get-string
                    :nwqids   #'get-nwqids
                    :wnames   #'get-wnames
                    :data     #'get-data
                    :tag      #'get-short
                    :oldtag   #'get-short
                    :fid      #'get-int
                    :afid     #'get-int
                    :newfid   #'get-int
                    :msize    #'get-int
                    :perm     #'get-int
                    :iounit   #'get-int
                    :iomode   #'get-byte
                    :offset   #'get-long
                    :count    #'get-int
                    :type     #'get-short
                    :dev      #'get-int
                    :qid-type #'get-byte
                    :qid-vers #'get-int
                    :qid-path #'get-long
                    :mode     #'get-int
                    :atime    #'get-int
                    :mtime    #'get-int
                    :length   #'get-long
                    :size     #'get-short
                    :ssize    #'get-short})

(def put-operation {:version  #'string->bytes
                    :name     #'string->bytes
                    :uname    #'string->bytes
                    :aname    #'string->bytes
                    :muid     #'string->bytes
                    :uid      #'string->bytes
                    :gid      #'string->bytes
                    :ename    #'string->bytes
                    :nwqids   #'qid->bytes
                    :wnames   #'wname->bytes
                    :data     #'bytecoll->bytes
                    :tag      #'short->bytes
                    :oldtag   #'short->bytes
                    :fid      #'int->bytes
                    :afid     #'int->bytes
                    :newfid   #'int->bytes
                    :msize    #'int->bytes
                    :perm     #'int->bytes
                    :iounit   #'int->bytes
                    :iomode   #'byte->bytes
                    :offset   #'long->bytes
                    :count    #'int->bytes
                    :type     #'short->bytes
                    :dev      #'int->bytes
                    :qid-type #'byte->bytes
                    :qid-vers #'int->bytes
                    :qid-path #'long->bytes
                    :mode     #'int->bytes
                    :atime    #'int->bytes
                    :mtime    #'int->bytes
                    :length   #'long->bytes
                    :size     #'short->bytes
                    :ssize    #'short->bytes})

;; we iterate over the keys in the buffer-operator map, and resolve functions for reading them.
(def reverse-frame-byte      (reverse-map frame-byte))
(def reverse-access-mode     (reverse-map access-mode))
(def reverse-permission-mode (reverse-map permission-mode))
(def reverse-file-mode       (reverse-map file-mode))
(def reverse-qt-mode         (reverse-map qt-mode))
