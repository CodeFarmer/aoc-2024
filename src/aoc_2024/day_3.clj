(ns aoc-2024.day-3
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(defn get-multiplies [astr]
  (let [ms (re-seq (re-pattern "mul\\((\\d{1,3}),(\\d{1,3})\\)") astr)]
    (if (empty? ms)
      [[]]
      (->> ms
           (map rest)
           (map intify-seq)))))

(defn sum-multiplies [mults]
  (reduce + (map #(apply * %) mults)))

(defn on-sections
  ([astr]
   (on-sections [] astr))
  ([acc astr]
   (if (empty? astr)
     acc
     (if-let [i (str/index-of astr "don't()")]
       (let [remainder (subs astr (+ i 7))
             j (str/index-of remainder "do()")
             acc' (conj acc (subs astr 0 i))]
         (if j (recur acc' (subs remainder j))
             acc'))
       (conj acc astr)))))
