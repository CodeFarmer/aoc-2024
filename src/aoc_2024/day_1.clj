(ns aoc-2024.day-1
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(defn lines-to-lists
  ([aseq]
   (reduce (fn [[avec bvec] astr]
             (let [[a b] (string-to-ints astr)]
               [(conj avec a) (conj bvec b)]))
           [[] []]
           aseq)))

(defn sorted-diffs [[aseq bseq]]
  (map abs (map - (sort aseq) (sort bseq))))

(defn similarity-scores [[aseq bseq]]
  (let [freqs (frequencies bseq)]
    (map #(* % (get freqs % 0)) aseq)))


