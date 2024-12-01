(ns aoc-2024.day-1-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-1 :refer :all]
            [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (str/split
   "3   4
4   3
2   5
1   3
3   9
3   3"
   #"\n"))

(deftest string-to-ints-test
  (is (= [4 3] (string-to-ints "4   3"))))

(deftest lines-to-lists-test
  (is (= [[] []] (lines-to-lists [])))
  (is (= [[1] [2]] (lines-to-lists ["1  2"])))
  (is (= [[3 4 2 1 3 3] [4 3 5 3 9 3]]
         (lines-to-lists sample-data))))

(deftest sorted-diffs-test
  (is (= [2 1 0 1 2 5] (sorted-diffs (lines-to-lists sample-data)))))

(def input-data
  (lines-as-vector "aoc-2024-inputs/input-1.txt"))

(deftest part-1-test
  (is (= 3714264 (reduce + (sorted-diffs (lines-to-lists input-data))))))

;; part 2

(deftest similarity-scores-test
  (is (= [9 4 0 0 9 9] (similarity-scores (lines-to-lists sample-data)))))

(deftest part-2-test
  (is (= 18805872 (reduce + (similarity-scores (lines-to-lists input-data))))))
