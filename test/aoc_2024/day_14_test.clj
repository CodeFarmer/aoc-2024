(ns aoc-2024.day-14-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-14 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  (str/split 
   "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"
   #"\n"))

(deftest parsing-test
  (is (= [[0 4] [3 -3]]
         (parse-robot (first sample-data)))))

(deftest tick-robot-test
  (is (= [[3 1] [3 -3]]
         (tick-robot [[0 4] [3 -3]])))
  (is (= [[6 5] [2 -3]]
         (->> [[2 4] [2 -3]]
              (tick-robot 11 7)
              (tick-robot 11 7)))))

(def sample-robots
  (map parse-robot sample-data))

(comment
  (square-robot-counts (nth (iterate #(map (partial tick-robot 11 7) %) sample-robots) 100)))

;; it turns out that the square-counting is not important

(deftest safety-factor-test
  (is (= 12 (safety-factor 7 11 100 sample-robots))))

(def input-robots
  (map parse-robot
       (str/split (slurp "aoc-2024-inputs/input-14.txt")
                  #"\n")))

(deftest part-1-test
  (is (= 220971520 (safety-factor 101 103 100 input-robots))))
