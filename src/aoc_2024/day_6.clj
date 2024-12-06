(ns aoc-2024.day-6
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))


(defn on-map? [tmap [x y]]
  (let [width (aoc/tmap-width tmap)
        height (aoc/tmap-height tmap)]
    (and (not (>= x width))
         (not (< x 0))
         (not (>= y height))
         (not (< y 0)))))

(defn guard-walk [tmap [x y] dir]
  (let [delta (aoc/directions dir)
        width (aoc/tmap-width tmap)
        height (aoc/tmap-height tmap)]
    (take-while (fn [[x' y']]
                  (and (not (> x' width))
                       (not (< x' -1))
                       (not (> y' height))
                       (not (< y' -1))
                       (not (= \# (aoc/get-tile tmap [x' y'])))))
                (iterate #(map + delta %) (map + delta [x y])))))

(def compass {:up :right
              :right :down
              :down :left
              :left :up})

(defn guard-route
  ([tmap start dir]
   (guard-route [] tmap start dir))
  ([acc tmap start dir]
   (let [walk (guard-walk tmap start dir)
         stop (last walk)]
     (if (not (on-map? tmap stop))
       (concat acc (butlast walk))
       (recur (concat acc walk) tmap stop (compass dir))))))

(defn guard-route-squares [tmap start dir]
  (into #{start} (guard-route tmap start dir)))
