(ns phlegyas.sqlitefs)

(defn calculate-block-info
  "Given an offset and a block size, calculate the block index and the
   position in the block."
  [offset block-size]
  (let [block-index (quot offset block-size)
        position-in-block (mod offset block-size)]
    {:block-index block-index, :position-in-block position-in-block}))
