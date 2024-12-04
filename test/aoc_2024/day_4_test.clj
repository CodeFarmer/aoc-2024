(ns aoc-2024.day-4-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-4 :refer :all]
            [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (into []
        (str/split
         "..X...
.SAMX.
.A..A.
XMAS.S
.X...." #"\n")))

(def bigger-sample
  (into []
        (str/split
         "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"
         #"\n")))

(def bigger-sample-revealed
  (into []
        (str/split
         "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX"
         #"\n")))

(deftest word-test
  (is (word? "XMAS" sample-data [2 0] [1 1])
      "XMAS should occur in the map starting at (2, 0) down and to the right")
  (is (not (word? "XMAS" sample-data [2 0] [1 0]))
      "XMAS should not occur in the map starting at (2, 0) to the right")
  (is (not (word? "XMAS" sample-data [2 0] [0 -1]))
      "XMAS should not occur in the map starting at (2, 0) going up, and it should also not cause an error"))

(deftest count-words-test
  (is (= 18 (count (find-occurrences "XMAS" bigger-sample)))))

(def input-data
  (lines-as-vector "aoc-2024-inputs/input-4.txt"))

(deftest part-1-test
  (is (= 2378 (count (find-occurrences "XMAS" input-data)))))

;; part 2

(deftest count-x-mas-test
  (is (= 9 (count (find-x-mases bigger-sample)))))

(deftest part-2-test
  (is (= 1796 (count (find-x-mases input-data)))))
