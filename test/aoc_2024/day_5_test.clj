(ns aoc-2024.day-5-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-5 :refer :all]
            [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(deftest get-pairs-test
  (is (= 21 (count (get-data-pairs sample-data))))
  (is (= [47 53] (first (get-data-pairs sample-data)))))

(deftest get-updates-test
  (is (= 6 (count (get-updates sample-data))))
  (is (= [75 47 61 53 29] (first (get-updates sample-data)))))

(deftest order-index-test
  (let [idx (build-index (get-data-pairs sample-data))]
    (is (before-ok? idx 75 47))
    (is (not (before-ok? idx 75 97)))))

(deftest ordered-update-test
  (let [idx (build-index (get-data-pairs sample-data))
        updates (get-updates sample-data)]
    (is (ordered-update? idx (first updates)))
    (is (not (ordered-update? idx (nth updates 3))))
    (is (not (ordered-update? idx (nth updates 4))))
    (is (not (ordered-update? idx (last updates))))))

(deftest find-middle-test
  (is (= 61 (find-middle [75 47 61 53 29]))))

(deftest sample-test
  (let [idx (build-index (get-data-pairs sample-data))
        updates (get-updates sample-data)]
    (is (= 143
           (reduce + (map find-middle (filter #(ordered-update? idx %) updates)))))))

(def input-data (slurp "aoc-2024-inputs/input-5.txt"))

(deftest part-1-test
  (let [idx (build-index (get-data-pairs input-data))
        updates (get-updates input-data)]
    (is (= 5391
           (reduce + (map find-middle (filter #(ordered-update? idx %) updates)))))))
