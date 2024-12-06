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

(defn blocked? [tmap loc]
  (#{\# \O} (aoc/get-tile tmap loc)))

(defn guard-walk [tmap [x y] dir]
  (let [delta (aoc/directions dir)
        width (aoc/tmap-width tmap)
        height (aoc/tmap-height tmap)]
    (map #(vector % dir)
         (take-while (fn [[x' y']]
                       (and (not (> x' width))
                            (not (< x' -1))
                            (not (> y' height))
                            (not (< y' -1))
                            (not (blocked? tmap [x' y']))))
                     (iterate #(map + delta %) (map + delta [x y]))))))

(def next-turn {:up :right
                :right :down
                :down :left
                :left :up})

(defn guard-route
  ([tmap start dir]
   (guard-route #{} tmap start dir))
  ([acc tmap start dir]
   (let [walk (guard-walk tmap start dir)
         stop (first (last walk))]
     (if (not (on-map? tmap stop))
       (into acc (butlast walk))
       (recur (into acc walk) tmap stop (next-turn dir))))))

(defn guard-route-squares [tmap start dir]
  (into #{start} (map first (guard-route tmap start dir))))

(defn guard-route-loop?
  ([seen tmap start dir]
   (let [walk (guard-walk tmap start dir)
         stop (first (last walk))]
     ;; TODO deal better with the fact that a walk can be zero
     (cond (nil? stop) (recur seen tmap start (next-turn dir))
           (not (on-map? tmap stop)) false
           (some seen walk) true
           :default (recur (into seen walk) tmap stop (next-turn dir))))))

(defn guard-route-counting-obstacles
  [tmap start dir]
  (let [route-squares (into #{} (map first (guard-route tmap start dir)))]
    (filter #(guard-route-loop? #{} (aoc/tmap-update tmap % \O) start dir) route-squares)))

(defn p-guard-route-counting-obstacles
  [tmap start dir]
  (let [route-squares (into #{} (map first (guard-route tmap start dir)))]
    (filter true? (pmap #(guard-route-loop? #{} (aoc/tmap-update tmap % \O) start dir) route-squares))))




