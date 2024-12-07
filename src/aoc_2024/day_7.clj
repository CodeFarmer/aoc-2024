(ns aoc-2024.day-7
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(defn parse-line [astr]
  (let [[h t] (str/split astr #": ")]
    [(bigint h) (aoc/intify-seq (str/split t #"\s+"))]))

(defn has-possibility?
  ([r operators aseq]
   (has-possibility? r (first aseq) operators (rest aseq)))
  ([r t operators aseq]
   (cond (empty? aseq) (= t r)
         (> t r) false
         :default (let [branches (map #(% t (first aseq)) operators)]
                    (some #(has-possibility? r % operators (rest aseq)) branches)))))

(defn calibration-total
  ([strings]
   (calibration-total [* +] strings))
  ([operators strings]
   (reduce +
           (map first 
                (filter (fn [[t s]] (has-possibility? t operators s))
                        (map parse-line strings))))))

(defn numcat [a b]
  (bigint (str a b)))
