(ns aoc-2024.day-12-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-12 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  (str/split
   "AAAA
BBCD
BBCC
EEEC"
   #"\n"))

(def tricky-sample
  (str/split
   "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"
   #"\n"))

(def larger-sample
  (str/split
   "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"
   #"\n"))

(deftest region-containing-test
  (is (= #{[0 0] [1 0] [2 0] [3 0]
           [0 1] [1 1] [2 1] [3 1]
                       [2 2] [3 2] [4 2]
                       [2 3]}
         (region-containing larger-sample [0 0]))))

(deftest find-regions-test
  (let [regions (find-regions sample-data)]
    (is (= 5 (count regions)))
    (is (= #{#{[2 2] [3 3] [2 1] [3 2]}
             #{[0 0] [1 0] [3 0] [2 0]}
             #{[1 1] [0 2] [1 2] [0 1]}
             #{[3 1]}
             #{[2 3] [1 3] [0 3]}}
           regions)))
  (let [regions (find-regions tricky-sample)]
    (is (= 5 (count regions)))
    (is (= #{#{[1 3]}
             #{[1 1]}
             #{[4 3] [2 2] [0 0] [1 0] [2 3] [3 4] [4 2] [3 0] [4 1] [1 4] [0 3] [2 4] [0 2] [2 0] [0 4] [2 1] [4 4] [1 2] [3 2] [0 1] [4 0]}
             #{[3 3]}
             #{[3 1]}}
           regions)))

  (let [regions (find-regions larger-sample)]
    (is (= 11 (count regions)))))

(deftest perimeter-test
  (let [regions (find-regions sample-data)]
    (is (= 10 (count-perimeter regions #{[0 0] [1 0] [3 0] [2 0]})))))


(deftest price-test
  (let [regions (find-regions sample-data)]
    (is (= 40 (price regions #{[0 0] [1 0] [3 0] [2 0]})))))

(deftest total-price-test
  (let [regions (find-regions sample-data)]
    (is (= 140 (total-price regions))))
  (let [regions (find-regions tricky-sample)]
    (is (= 772 (total-price regions))))
  (let [regions (find-regions larger-sample)]
    (is (= 1930 (total-price regions)))))

(def input-data
  (str/split
   (slurp "aoc-2024-inputs/input-12.txt")
   #"\n"))

(deftest part-1-test
  (let [regions (find-regions input-data)]
    (comment (output-regions input-data))
    (is (= 1361494 (total-price (find-regions input-data))))))
