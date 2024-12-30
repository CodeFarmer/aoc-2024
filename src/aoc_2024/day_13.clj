(ns aoc-2024.day-13
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]
            [clojure.math :as math]
            [clojure.core.matrix :as ma]
            [clojure.core.matrix.implementations :as mi]
            [clojure.core.matrix.linear :as ml]))

;; Button A: X+94, Y+34
;; Button B: X+22, Y+67
;; Prize: X=8400, Y=5400
(defn parse-machine [astr]
  (map aoc/intify-seq
       (map #(re-seq #"\d+" %) (str/split astr #"\n"))))


;; 94A + 22B = 8400
;; 34A + 67B = 5400
;; Cost = 3A + B

;; button A costs 3 tokens, button B costs 1 token
;; how cheaply can I reach the prize?
;; (return 0 if the prize is unreachable)

(mi/set-current-implementation :vectorz)

(def EPSILON 0.0001)

;; return nil if afloat is not within EPSILON of the nearest integer,
;; otherwise return the nearest integer
(defn ftoi [afloat]
  (let [i (math/round afloat)]
    (if (> EPSILON (abs (- afloat i)))
      i
      nil)))

(defn integer-solution [avec]
  (let [v (mapv ftoi avec)]
    (if (every? #(and 
                  (not (nil? %))
                  (not (> 0 %))) v)
      v
      nil)))

(defn minimum-cost [[[ax ay]
                     [bx by] target]]
  (let [X [[ax bx]
           [ay by]]
        sol (ml/solve X target)
        sol' (integer-solution sol)]
    (if sol'
      (let [[a b] sol']
        (assert (= [(+ (* a ax) (* b bx))
                    (+ (* a ay) (* b by))] target))
        (+ (* 3 a) b))
      0)))

