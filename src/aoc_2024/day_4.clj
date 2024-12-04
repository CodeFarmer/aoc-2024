(ns aoc-2024.day-4
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))


(defn word? [astr tmap [x y] [dx dy]]
  (comment   (println "word?" astr [x y] [dx dy])
             (println "tile:" (get-tile tmap [x y])))
  (if (empty? astr) true
    (and (= (first astr) (get-tile tmap [x y]))
         (word? (rest astr) tmap [(+ x dx) (+ y dy)] [dx dy]))))

(defn find-locations [tmap achar]
  (for [y (range 0 (tmap-height tmap))
        x (range 0 (tmap-width tmap))
        :when (= achar (get-tile tmap [x y]))]
    [x y]))

(def all-directions
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (= 0 dx dy))]
    [dx dy]))

(defn find-occurrences [astr tmap]
  "find all pairs of (start, direction) that identify the places astr occurs in the grid"
  (for [start (find-locations tmap (first astr))
        dir all-directions
        :when (word? astr tmap start dir)]
    [start dir]))

(defn find-x-mases [tmap]
  "find all locations of the middle A in crossed MASes (in whatever direction)"
  (filter (fn [[x y]]
            (and (or (word? "MAS" tmap [(dec x) (dec y)] [ 1 1])
                     (word? "MAS" tmap [(inc x) (inc y)] [-1 -1]))
                 (or (word? "MAS" tmap [(dec x) (inc y)] [ 1 -1])
                     (word? "MAS" tmap [(inc x) (dec y)] [-1  1]))))
          (find-locations tmap \A)))
