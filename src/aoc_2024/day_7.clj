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
   (count-possibilities r [* +] aseq))
  ([r operators aseq]
   (count-possibilities r (first aseq) operators (rest aseq)))
  ([r t operators aseq]
   (cond (empty? aseq) (if (= t r) 1 0)
         (> t r) 0
         :default (let [branches (map #(% t (first aseq)) operators)]
                    (reduce + (map #(count-possibilities r % operators (rest aseq)) branches))))))

(defn calibration-total
  ([strings]
   (calibration-total [* +] strings))
  ([operators strings]
   (reduce +
           (map first 
                (filter (fn [[t s]] (> (count-possibilities t operators s) 0))
                        (map parse-line strings))))))

(defn numcat [a b]
  (bigint (str a b)))
