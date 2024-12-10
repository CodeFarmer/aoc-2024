(ns aoc-2024.day-9
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(defn parse-fs
  ([astr]
   (parse-fs [] 0 :file astr))
  ([acc current-id mode astr]
   (if (empty? astr) acc
       (let [size (parse-long (str (first astr)))
             mode' (if (= :file mode) :empty :file)
             id' (if (= :file mode) (inc current-id) current-id)]
         (if (= 0 size)
           (recur acc id' mode' (rest astr))
           (recur (conj acc (if (= :file mode)
                              {:id current-id :size size}
                              {:empty :empty :size size}))
                  id'
                  mode'
                  (rest astr)))))))

(defn fs-string
  ([fs]
   (fs-string "" fs))
  ([buf fs]
   (if (empty? fs)
     buf
     (let [obj (first fs)
           s (:size obj)
           r (rest fs)]
       (if (:empty obj)
         (recur (str buf (apply str (repeat s \.))) r)
         ;; this will only work with single-digit IDs eh
         (recur (str buf (apply str (repeat s (:id obj)))) r))))))


(defn crush-empty-blocks
  ([fs]
   (crush-empty-blocks [] 0 fs))
  ([acc empty-len fs]
   (if (empty? fs)
     (if (= 0 empty-len)
       acc
       (conj acc {:empty :empty :size empty-len}))
     (let [b (first fs)]
       (if (:empty b)
         (recur acc (+ empty-len (:size b)) (rest fs))
         (if (= 0 empty-len)
           (recur (conj acc b) 0 (rest fs))
           (recur (into acc [{:empty :empty :size empty-len} b]) 0 (rest fs))))))))

;; THIS IS SUPER DUMB. Actually doing it in steps was dumb

;; FOLLOWUP: This is some of the worst code I have ever written in any
;; language it's awful CLojure, and CLojure makes the solution more
;; awful Maybe I can rethink this one on the plane to Australa, since
;; I have an answer

(defn compact-step [fs]
  (let [compacted (take-while #(not (:empty %)) fs)
        uncompacted (drop-while #(not (:empty %)) fs)
        empty-block (first uncompacted)
        empty-block-size (:size empty-block)
        last-file (last (filter :id fs))
        last-file-id (:id last-file)
        last-file-size (:size last-file)
        tail (last uncompacted)
        ;; this is awful, don't code drunk or tired people
        trail-space (if (:empty tail) tail {:empty :empty :size 0})]
    (crush-empty-blocks
     (cond (< empty-block-size last-file-size)
           ;; move part of the end file inward
           (concat compacted
                   [{:id last-file-id :size empty-block-size}]
                   (take-while #(not (= last-file-id (:id %))) (rest uncompacted))
                   [{:id last-file-id :size (- last-file-size empty-block-size)} empty-block trail-space])
           (= empty-block-size last-file-size)
           ;; swap the end file for the empty block
           (concat compacted
                   [last-file]
                   (take-while #(not (= last-file-id (:id %))) (rest uncompacted))
                   [empty-block trail-space])
           :default
           ;; move the end file inward, consume part of the empty block
           (concat compacted
                   [last-file {:empty :empty :size (- empty-block-size last-file-size)}]
                   (take-while #(not (= last-file-id (:id %))) (rest uncompacted))
                   [{:empty :empty :size last-file-size} trail-space])
           ))))

(defn compact-totally
  ([fs]
   (compact-totally 0 fs))
  ([i fs]
   (if (= 0 (mod i 1000))
     (println "Compact" i ":" (count (take-while :id fs)) "/" (count fs)))
   (if (every? :id (butlast fs))
     fs
     (recur (inc i) (compact-step fs)))))

(defn checksum
  ([fs]
   (checksum 0 0 fs))
  ([acc idx fs]
   (if (empty? fs)
     acc
     (let [b (first fs)
           size (:size b)]
       (if (:empty b)
         (recur acc (+ idx size) (rest fs))
         (recur (+ acc
                   (reduce +
                           (map #(* (:id b) %)
                                (range idx (+ idx size)))))
                (+ idx size)
                (rest fs)))))))

;; part 2

;; TODO: write a partition-by that only finds the first true return, with before and after

(defn compact-nofrag
  ([fs]
   (compact-nofrag fs (apply max (map :id (filter :id fs)))))
  ([fs id]
   (if (= 0 (mod id 100))
     (println "Compact-nofrag:" id))
   (if (= id 0)
     fs
     (let [[left [b] right] (partition-by #(= id (:id %)) fs)
           b-size (:size b)
           left' (take-while #(or (:id %) (< (:size %) b-size)) left)
           right' (drop-while #(or (:id %) (< (:size %) b-size)) left)
           f (first right')]
       (if (not f)
         (recur fs (dec id))
         (recur (into [] ;; this is there so all the lazy seq take-whiles don't overflow at the end
                      (concat left'
                              [b {:empty :empty :size (- (:size f) b-size)}]
                              (rest right')
                              [{:empty :empty :size b-size}]
                              right))
                (dec id)))))))
