(ns aoc-2024.day-4-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-4 :refer :all]
            [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  "..X...
.SAMX.
.A..A.
XMAS.S
.X....")

(def bigger-sample
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def bigger-sample-revealed
  "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX")

(deftest count-words-test
  (is (= 18 (count-occurrences "XMAS" bigger-sample))))
