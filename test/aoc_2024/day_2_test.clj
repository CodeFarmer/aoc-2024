(ns aoc-2024.day-2-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-2 :refer :all]
            [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (map vec 
       (map string-to-ints
            (str/split "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9"
                       #"\n"))))

(deftest safe-test
  (is (safe? []))
  (is (safe? [1 2]))
  (is (not (safe? [1 1])) "A gap of less than one is unsafe")
  (is (not (safe? [1 2 2])) "Unsafety is not only at the front")
  (is (not (safe? [1 5])) "A gap of more than three is unsafe")
  (is (safe? [2 1]) "A gap can be decreasing")
  (is (not (safe? [5 1])) "A gap of more than three decrasing is unsafe")
  (is (not (safe? [1 3 5 4])) "Gaps must all be in the same direction")

  (is (= [true false false false false true]
         (map safe? sample-data))))

;; part 1

(def input-data (map vec (map string-to-ints (lines-as-vector "aoc-2024-inputs/input-2.txt"))))

(deftest part-1-test
  (is (= 572 (count (filter safe? input-data)))))

;; part 2

(deftest step-removal-test
  (is (= [[]]) (remove-each-once []))
  (is (= [[2] [1]] (remove-each-once [1 2])))
  (is (= [[2 3 4] [1 3 4] [1 2 4] [1 2 3]]
         (remove-each-once [1 2 3 4]))))

(deftest damping-test
  (is (damping-safe? [1 3 2 4 5]))
  (is (not (damping-safe? [9 7 6 2 1])))

  (is (= [true false false true true true]
         (map damping-safe? sample-data))))

(deftest part-2-test
  (is (= 612 (count (filter damping-safe? input-data)))))
