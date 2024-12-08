(ns aoc-2024.day-8
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]
            [clojure.math.combinatorics :as cc]))

(defn find-antinodes [a b]
  (let [delta (map - a b)]
    [(map + a delta) (map - b delta)]))

(defn find-antinode-group [tmap aseq]
  (into #{}
        (filter (partial aoc/on-map? tmap)
                (apply concat (map #(apply find-antinodes %) (cc/combinations aseq 2))))))

(def letters
  (map char (concat (range (int \a) (inc (int \z)))
                    (range (int \A) (inc (int \Z))))))
(def digits
  (map char (range (int \0) (inc (int \9)))))

(defn collect-antennae [tmap]
  (reduce (fn [acc c]
            (let [group (find-antinode-group tmap (aoc/tmap-find-locations tmap c))]
              (if (empty? group) acc
                  (assoc acc c group))))
          {}
          (concat letters digits)))
