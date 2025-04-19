(ns aoc-2024.day-16-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-16 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def example-map
  (aoc/tmap "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"))

(deftest starting-state-test
  (is (= [1 13 :right] (starting-state example-map))
      "The journey starts at the point marked S on the map, facing East (right)"))

(deftest finish-square-test
  (is (= [13 1] (finish-square example-map))))

(deftest possible-moves-test
  (is (= [[[1 13 :up] [#{} 1000]]
          [[1 13 :down] [#{} 1000]]
          [[2 13 :right] [#{[2 13]} 1]]]
         (possible-moves example-map [(starting-state example-map) [#{} 0]]))
      "Possible moves from the start are turn 90 degrees left or right, or step forward, with appropriate costs")

  (is (= [[[3 13 :up] [#{} 1003]]
          [[3 13 :down] [#{} 1003]]]
         (possible-moves example-map [[3 13 :right] [#{} 3]]))
      "Possible moves are turn 90 degrees left or right, where stepping forward is not possible"))


(def bigger-map
  (aoc/tmap "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"))

(deftest path-scoring-test
  (is (= 7036 (best-path-score example-map)))
  (is (= 11048 (best-path-score bigger-map))))

(def input-map
  (aoc/tmap (slurp "aoc-2024-inputs/input-16.txt")))
 
(deftest part-1-test
  (is (= 65436 (best-path-score input-map))))

;; part 2

(deftest bug-hunting-test
  (let [buggy-map
        (aoc/tmap "######
###E.#
#...##
#.#.##
#....#
#.####
#S..##
######")]
    (is (= 11 (count-best-path-tiles buggy-map)))))

(deftest path-counting-test
  (is (= 45 (count-best-path-tiles example-map)))
  (is (= 64 (count-best-path-tiles bigger-map))))

;; TODO run one walk for parts 1 and 2, this is ridiculous
(deftest part-2-test
  (is (= 489 (count-best-path-tiles input-map))))
