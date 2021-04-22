(ns phlegyas.types
  (:require [phlegyas.util :refer [reverse-map]]
            [phlegyas.buffers :as buffers]))

; protocol defaults
(def protocol-version   "9P2000")
(def notag                0xffff)
(def nofid            0xffffffff)
(def stat-keep-number 0xffffffff)
(def max-message-size 0x7fffffff)
(def stat-keep-string         "")

(def access-mode {:oread   0x00
                  :owrite  0x01
                  :ordrw   0x02
                  :oexec   0x03
                  :otrunc  0x10
                  :orclose 0x40
                  :oexcl   0x1000})

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

(def frame-layouts {:Tversion [:tag :msize :version]
                    :Rversion [:tag :msize :version]

                    :Tauth    [:tag :afid :uname :aname]
                    :Rauth    [:tag :qid-type :qid-vers :qid-path]

                    :Rerror   [:tag :ename]

                    :Tflush   [:tag :oldtag]
                    :Rflush   [:tag]

                    :Tattach  [:tag :fid :afid :uname :aname]
                    :Rattach  [:tag :qid-type :qid-vers :qid-path]

                    :Twalk    [:tag :fid :newfid :wnames]
                    :Rwalk    [:tag :nwqids]

                    :Topen    [:tag :fid :iomode]
                    :Ropen    [:tag :qid-type :qid-vers :qid-path :iounit]

                    :Tcreate  [:tag :fid :name :perm :iomode]
                    :Rcreate  [:tag :qid-type :qid-vers :qid-path :iounit]

                    :Tread    [:tag :fid :offset :count]
                    :Rread    [:tag :data]

                    :Twrite   [:tag :fid :offset :data]
                    :Rwrite   [:tag :count]

                    :Tclunk   [:tag :fid]
                    :Rclunk   [:tag]

                    :Tremove  [:tag :fid]
                    :Rremove  [:tag]

                    :Tstat    [:tag :fid]
                    :Rstat    [:tag :ssize :size :type :dev :qid-type :qid-vers :qid-path :mode :atime :mtime :length :name :uid :gid :muid]

                    :Twstat   [:tag :fid :ssize :size :type :dev :qid-type :qid-vers :qid-path :mode :atime :mtime :length :name :uid :gid :muid]
                    :Rwstat   [:tag]})

(def type-size {:tag      2
                :oldtag   2
                :msize    4
                :size     2
                :ssize    2
                :fid      4
                :afid     4
                :newfid   4
                :perm     4
                :iounit   4
                :offset   8
                :type     2
                :dev      4
                :qid-type 1
                :qid-vers 4
                :qid-path 8
                :iomode   1
                :mode     4
                :atime    4
                :mtime    4
                :length   8
                :count    4})

(def get-operation {:version  #'buffers/get-string
                    :name     #'buffers/get-string
                    :uname    #'buffers/get-string
                    :aname    #'buffers/get-string
                    :muid     #'buffers/get-string
                    :uid      #'buffers/get-string
                    :gid      #'buffers/get-string
                    :ename    #'buffers/get-string
                    :nwqids   #'buffers/get-nwqids
                    :wnames   #'buffers/get-wnames
                    :data     #'buffers/get-data
                    :tag      #'buffers/get-short
                    :oldtag   #'buffers/get-short
                    :fid      #'buffers/get-int
                    :afid     #'buffers/get-int
                    :newfid   #'buffers/get-int
                    :msize    #'buffers/get-int
                    :perm     #'buffers/get-int
                    :iounit   #'buffers/get-int
                    :iomode   #'buffers/get-byte
                    :offset   #'buffers/get-long
                    :count    #'buffers/get-int
                    :type     #'buffers/get-short
                    :dev      #'buffers/get-int
                    :qid-type #'buffers/get-byte
                    :qid-vers #'buffers/get-int
                    :qid-path #'buffers/get-long
                    :mode     #'buffers/get-int
                    :atime    #'buffers/get-int
                    :mtime    #'buffers/get-int
                    :length   #'buffers/get-long
                    :size     #'buffers/get-short
                    :ssize    #'buffers/get-short})

(def put-operation {:version  #'buffers/put-string
                    :name     #'buffers/put-string
                    :uname    #'buffers/put-string
                    :aname    #'buffers/put-string
                    :muid     #'buffers/put-string
                    :uid      #'buffers/put-string
                    :gid      #'buffers/put-string
                    :ename    #'buffers/put-string
                    :nwqids   #'buffers/put-qid
                    :wnames   #'buffers/put-wname
                    :data     #'buffers/put-bytecoll
                    :tag      #'buffers/put-short
                    :oldtag   #'buffers/put-short
                    :fid      #'buffers/put-int
                    :afid     #'buffers/put-int
                    :newfid   #'buffers/put-int
                    :msize    #'buffers/put-int
                    :perm     #'buffers/put-int
                    :iounit   #'buffers/put-int
                    :iomode   #'buffers/put-byte
                    :offset   #'buffers/put-long
                    :count    #'buffers/put-int
                    :type     #'buffers/put-short
                    :dev      #'buffers/put-int
                    :qid-type #'buffers/put-byte
                    :qid-vers #'buffers/put-int
                    :qid-path #'buffers/put-long
                    :mode     #'buffers/put-int
                    :atime    #'buffers/put-int
                    :mtime    #'buffers/put-int
                    :length   #'buffers/put-long
                    :fsize    #'buffers/put-int
                    :ftype    #'buffers/put-byte
                    :size     #'buffers/put-short
                    :ssize    #'buffers/put-short})

;; we iterate over the keys in the buffer-operator map, and resolve functions for reading them.
(def reverse-frame-byte      (reverse-map frame-byte))
(def reverse-access-mode     (reverse-map access-mode))
(def reverse-permission-mode (reverse-map permission-mode))
(def reverse-file-mode       (reverse-map file-mode))
(def reverse-qt-mode         (reverse-map qt-mode))
