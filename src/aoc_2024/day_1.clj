(ns aoc-2024.day-1
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))


(defn string-to-ints [astr]
  (intify-seq (str/split astr #"\s+")))

(defn lines-to-lists
  ([aseq]
   (reduce (fn [[avec bvec] astr]
             (let [[a b] (string-to-ints astr)]
               [(conj avec a) (conj bvec b)]))
           [[] []]
           aseq)))

(defn sorted-diffs [[aseq bseq]]

  (->> (map - (sort aseq) (sort bseq))
       (map abs)
       (reduce +)))

(defn count-occurrences [aseq x]
  (count (filter #(= x %) aseq)))

(defn similarity-scores [[aseq bseq]]
  (map #(* % (count-occurrences bseq %)) aseq))


