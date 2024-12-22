(ns aoc-2024.day-12
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(defn region-containing
  "Find by flood fill all contiguous squares with the same tile, and return as a set"
  ([tmap point]
   (comment (println "region-containing" point (aoc/get-tile tmap point)))
   (region-containing #{}
                      clojure.lang.PersistentQueue/EMPTY
                      (aoc/get-tile tmap point)
                      tmap
                      point))
  ([acc q t tmap point]
   (let [acc' (conj acc point)
         q' (into q (filter #(and (not (some #{%} q))
                                  (not (acc' %))
                                  (= t (aoc/get-tile tmap %)))
                            (aoc/tmap-find-neighbours point tmap)))]
     (if (empty? q') acc'
         (recur acc' (pop q') t tmap (peek q'))))))

(defn find-regions [tmap]
  (reduce (fn [acc point]
            (if (some #(% point) acc) acc
                (conj acc (region-containing tmap point))))
          #{}
          (for [y (range 0 (aoc/tmap-height tmap))
                x (range 0 (aoc/tmap-width tmap))]
            [x y])))

(defn find-virtual-neighbours [point]
  (map #(map + point %) aoc/neighbour-deltas))

(defn count-perimeter
  [regions region]
  (count 
   (filter
    (fn [p]
      (not (region p)))
    (apply concat (map find-virtual-neighbours region)))))

(defn price [regions region]
  (* (count region) (count-perimeter regions region)))

(defn total-price [regions]
  (reduce + (map (partial price regions) regions)))

(defn output-regions [tmap]
  (let [regions (find-regions tmap)]
    (doseq [r regions]
      (println (aoc/get-tile tmap (first r)) ":" (count r) "x" (count-perimeter regions r) "=" (price regions r)))))
