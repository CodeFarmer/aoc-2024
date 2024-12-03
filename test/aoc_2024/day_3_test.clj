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
  (is (= 161 (sum-multiplies (get-multiplies sample-data)))))

(def input-data
  (slurp "aoc-2024-inputs/input-3.txt"))

(deftest part-1-test
  (is (= 164730528 (sum-multiplies (get-multiplies input-data)))))

;; part 2

(def new-sample-data "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


(deftest on-sections-test
  (is (= [] (on-sections "")))
  (is (= ["footwerpy"] (on-sections "footwerpy")))
  (is (= ["footwerpy"] (on-sections "footwerpydon't()graap")))
  (is (= ["xmul(2,4)&mul[3,7]!^" "do()?mul(8,5))"] (on-sections new-sample-data))))
