(ns aoc-2024.day-5
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(defn get-data-pairs [astr]
  (map #(intify-seq (str/split % (re-pattern "\\|")))
   (str/split (first (str/split astr #"\n\n"))
              #"\n")))

(defn get-updates [astr]
  (map #(intify-seq (str/split % (re-pattern ",")))
   (str/split (second (str/split astr #"\n\n"))
          #"\n")))

(defn build-index [pairs]
  (into #{} pairs))

(defn before-ok? [idx a b]
  (not (idx [b a])))

(defn ordered-update?
  ([idx update]
   (ordered-update? idx (first update) (rest update)))
  ([idx n update]
   (if (empty? update) true
       (and (before-ok? idx n (first update))
            (recur idx (first update) (rest update))))))

(defn find-middle [alist]
  (nth alist (quot (count alist) 2)))

;; part 1

