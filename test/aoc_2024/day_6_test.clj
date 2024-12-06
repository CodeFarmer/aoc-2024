(ns aoc-2024.day-6-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-6 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  (str/split "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
             #"\n"))


(deftest walking-test
  (is (= [[4 5] [4 4] [4 3] [4 2] [4 1]] (map first (guard-walk sample-data [4 6] :up)))
      "Guard should walk until an obstacle occurs")
  (is (= [[5 6] [6 6] [7 6] [8 6] [9 6] [10 6]] (map first (guard-walk sample-data [4 6] :right)))
      "Guard should walk one space off the edge if there is no obstacle"))

(deftest guard-routing-test
  (is (= 41 (count (guard-route-squares sample-data [4 6] :up)))))

(def input-data
  (str/split
   (slurp "aoc-2024-inputs/input-6.txt")
   #"\n"))

(deftest part-1-test
  (is (= 4903 (count (guard-route-squares input-data (first (aoc/tmap-find-locations input-data \^)) :up)))))

;; part 2

(deftest loop-test
  (is (not (guard-route-loop? #{} sample-data [4 6] :up))
      "The initial sample should not loop")
  (is (guard-route-loop? #{} (aoc/tmap-update sample-data [3 6] \O) [4 6] :up)
      "Placing an obstacle to the right of the starting position should induce a loop")
  (is (guard-route-loop? #{} (aoc/tmap-update sample-data [6 7] \O) [4 6] :up)
      "Placing an obstacle in the lower right quadrant should induce a loop"))

(deftest obstacle-placing-test
  (let [obstacles (guard-route-counting-obstacles sample-data [4 6] :up)]
    (comment 
      (println "obstacles:" obstacles)
      (doseq [m (map #(aoc/tmap-update sample-data % \O) obstacles)]
        (aoc/print-tmap m)
        (println)))
    (is (= 6 (count obstacles)))))

(deftest part-2-test
  (is (= 1911 (count (p-guard-route-counting-obstacles input-data (first (aoc/tmap-find-locations input-data \^)) :up)))))
