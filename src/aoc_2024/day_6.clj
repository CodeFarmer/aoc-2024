(ns aoc-2024.day-6
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

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
   (guard-route [] tmap start dir))
  ([acc tmap start dir]
   (let [walk (guard-walk tmap start dir)
         stop (first (last walk))]
     (if (not (aoc/on-map? tmap stop))
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
           (not (aoc/on-map? tmap stop)) false
           (some seen walk) true
           :default (recur (into seen walk) tmap stop (next-turn dir))))))

(defn guard-route-counting-obstacles
  [tmap start dir]
  (let [route-squares (guard-route-squares tmap start dir)]
    (filter #(guard-route-loop? #{} (aoc/tmap-update tmap % \O) start dir) route-squares)))

(defn guard-route-counting-obstacles-p
  [tmap start dir]
  (let [route-squares (into #{} (map first (guard-route tmap start dir)))]
    (filter true? (pmap #(guard-route-loop? #{} (aoc/tmap-update tmap % \O) start dir) route-squares))))

;; experimental TODO: figure out why this much faster way wrongly
;; returns ~130 extra obstacles

(defn guard-route-counting-obstacles-i
  [tmap start dir]
  (loop [acc #{}
         seen #{}
         route (guard-route tmap start dir)
         loc start
         dir dir]
    (if (empty? route)
      acc
      (let [[l d] (first route)]
        (if (guard-route-loop? (conj seen [loc dir]) (aoc/tmap-update tmap l \O) loc dir)
          (recur (conj acc l)
                 (conj seen [loc dir])
                 (rest route)
                 l
                 d)
          (recur acc
                 (conj seen [loc dir])
                 (rest route)
                 l
                 d))))))


