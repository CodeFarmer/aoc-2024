(ns aoc-2024.day-7
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(defn parse-line [astr]
  (let [[h t] (str/split astr #": ")]
    [(bigint h) (aoc/intify-seq (str/split t #"\s+"))]))


;; Count the possible ways the numbers in aseq can be joined by
;; operators to give the result r
(defn count-possibilities
  ([r aseq]
   (count-possibilities r (first aseq) (rest aseq)))
  ([r t aseq]
   (if (empty? aseq)
     (if (= t r) 1 0)
     (let [branches (map #(% t (first aseq)) [* +])]
       (reduce + (map #(count-possibilities r % (rest aseq)) branches))))))
