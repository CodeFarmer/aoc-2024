(ns aoc-2024.day-10-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-10 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  (str/split 
   "0123
1234
8765
9876"
   #"\n"))

;; a hiking trail is any path that starts at height 0, ends at height
;; 9, and always increases by a height of exactly 1 at each step

(deftest find-next-steps-test
  (is (= [[1 0] [0 1]]
         (find-next-steps sample-data [0 0])))
  (is (= [[0 2] [1 3]]
         (find-next-steps sample-data [1 2]))))

(def bigger-sample
  (str/split
   "...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9"
   #"\n"))

(deftest search-test
  (is (= #{[0 3]}
         (reachable-goals sample-data [0 0])))
  (is (= #{[0 6] [6 6]}
         (reachable-goals bigger-sample [3 0]))))

(def more-path-sample
  (str/split
   "..90..9
...1.98
...2..7
6543456
765.987
876....
987...."
   #"\n"))

(deftest path-counting-test
  (is (= 4 (count (reachable-goals more-path-sample [3 0])))))

(def multiple-start-sample
  (str/split 
   "10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01"
   #"\n"))

(def larger-example
  (str/split
   "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"
   #"\n"))

(deftest scoring-test
  (is (= 3 (score multiple-start-sample)))
  (is (= 36 (score larger-example))))

(def input-data
  (aoc/lines-as-vector "aoc-2024-inputs/input-10.txt"))

(deftest part-1-test
  (is (= 0 (score input-data))))

;; part 2

(deftest part-1-test
  (is (= 1735 (rating input-data))))
