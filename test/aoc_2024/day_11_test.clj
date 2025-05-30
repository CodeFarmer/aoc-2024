(ns aoc-2024.day-11-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-11 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data [0 1 10 99 999])

(deftest rules-test
  (is (= [1] (blink 0)))
  (is (= [2024] (blink 1)))
  (is (= [1 0] (blink 10)))
  (is (= [9 9] (blink 99)))
  (is (= [2021976] (blink 999))))

(deftest blink-stones-test
  (is (= [1 2024 1 0 9 9 2021976] (blink-stones sample-data))))

(deftest longer-example-test
  (is (= [2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2]
         (nth (iterate blink-stones [125 17]) 6)))
  (is (= 55312 (count-after-blinks [125 17] 25))))


(def input-data
  (map parse-long
       (str/split
        (slurp "aoc-2024-inputs/input-11.txt")
        #"\s+")))

(deftest part-1-test
  (is (= 203457 (count-after-blinks input-data 25))))

;; part 2

(deftest iterated-blink-count-test
  (is (= 55312 (iterated-blink-count-seq 25 [125 17]))))

(deftest part-2-test
  (is (= 241394363462435N (iterated-blink-count-seq 75 input-data))))
