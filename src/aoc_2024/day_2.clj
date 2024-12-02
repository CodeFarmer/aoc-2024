(ns aoc-2024.day-2
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))

(defn -pair-safety [sign x y]
  ;; return the sign if safe, false otherwise
  (let [d (- y x)
        d' (abs d)]
    (cond (= x y) false
          (> d' 3) false
          :default (let [signd (/ d d')]
                     (if (and sign (not (= sign signd))) false
                         signd)))))

(defn -safe?
  [sign x aseq]
  (if (empty? aseq) true
      (let [y (first aseq)
            s (-pair-safety sign x y)]
        (if (not s)
          false
          (recur s y (rest aseq))))))

(defn safe? [aseq]
  (-safe? nil (first aseq) (rest aseq)))

;; It's day 2, brute force is actively encouraged

(defn remove-each-once [avec]
  (for [i (range 0 (count avec))]
    (concat (subvec avec 0 i) (subvec avec (inc i)))))

(defn damping-safe? [aseq]
  (if (safe? aseq) true
      (true? (some safe? (remove-each-once aseq)))))
