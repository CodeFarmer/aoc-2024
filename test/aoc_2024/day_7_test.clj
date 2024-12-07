(ns aoc-2024.day-7-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-7 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  (str/split 
   "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"
   #"\n"))

(deftest parsing-test
  (is (= [190 [10 19]] (parse-line (first sample-data)))))

(deftest possibilities-test
  (is (= 0 (count-possibilities 190 [])))
  (is (= 0 (count-possibilities 190 [20])))
  (is (= 1 (count-possibilities 190 [190])))
  (is (= 1 (count-possibilities 190 [190 0])))
  (is (= 2 (count-possibilities 3267 [81 40 27])))
  (is (= 1 (count-possibilities 292 [11 6 16 20]))))

(deftest count-calibration-results-test
  (is (= 3749 (calibration-total sample-data))))

(def input-data
  (str/split (slurp "aoc-2024-inputs/input-7.txt")
             #"\n"))

(deftest part-1-test
  (is (= 3598800864292N (calibration-total input-data))))

;; part 2

(deftest numcat-test
  (is (= 12345 (numcat 12 345))))

(is (= 11387
       (calibration-total [* + numcat] sample-data)))

(is (= 340362529351427N
       (calibration-total [* + numcat] input-data)))
