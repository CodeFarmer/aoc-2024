(ns aoc-2024.day-3-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-3 :refer :all]
            [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(def sample-data "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(deftest get-multiplies-test
  (is (= [[]] (get-multiplies "")))
  (is (= [[1 2]] (get-multiplies "mul(1,2)")))
  (is (= [[2 4] [5 5] [11 8] [8 5]] (get-multiplies sample-data))))

(deftest summing-test
  (is (= 161 (reduce + (map #(apply * %) (get-multiplies sample-data))))))

(def input-data
  (slurp "aoc-2024-inputs/input-3.txt"))

(deftest part-1-test
  (is (= 164730528 (reduce + (map #(apply * %) (get-multiplies input-data))))))
