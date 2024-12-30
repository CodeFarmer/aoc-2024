(ns aoc-2024.day-13-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-13 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  (str/split 
   "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"
   #"\n\n"))

;; a machine looks like
;; [[adx ady] [bdx bdy] [prizex prizey]]
(deftest parsing-test
  (let [machines (map parse-machine sample-data)]
    (is (= 4 (count machines)))
    (is (= [[94 34] [22 67] [8400 5400]]
           (first machines)))
    (is (= [[69 23] [27 71] [18641 10279]]
           (last machines)))))

(deftest cheapest-way-test
  (let [machines (map parse-machine sample-data)]
    (is (= 280 (minimum-cost (first machines))))
    (is (= 0 (minimum-cost (second machines))))
    (is (= 200 (minimum-cost (nth machines 2))))
    (is (= 0 (minimum-cost (nth machines 3))))))

(def input-data
  (str/split (slurp "aoc-2024-inputs/input-13.txt")
             #"\n\n"))

(def input-machines
  (map parse-machine input-data))

(deftest part-1-test
  (is (= 31761 (reduce + (map minimum-cost input-machines)))))

;; part 2

(defn correct-xy
  [[a b [x y]]] [a b [(+ 10000000000000 x)
                      (+ 10000000000000 y)]])

(def part-2-machines (map correct-xy
                          input-machines))

(deftest part-2-test
  (is (= 90798500745591 (reduce + (map minimum-cost part-2-machines)))))
