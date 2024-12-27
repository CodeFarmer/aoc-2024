(ns aoc-2024.day-12
  (:require [aoc-2024.core :as aoc]
            [clojure.set :as s]
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

(defn count-perimeter [region]
  (count 
   (filter
    (fn [p]
      (not (region p)))
    (apply concat (map find-virtual-neighbours region)))))

(defn price [region]
  (* (count region) (count-perimeter region)))

(defn total-price [regions]
  (reduce + (map price regions)))

(defn output-regions [tmap]
  (let [regions (find-regions tmap)]
    (doseq [r regions]
      (println (aoc/get-tile tmap (first r)) ":" (count r) "x" (count-perimeter regions r) "=" (price regions r)))))

;; part 2

;; an edge is a pair of squares, one inside, one outside
;; a side is a contiguous run of edges

;; 1. find all the squares that have outside neighbours and store them
;; as pairs

(defn find-border-pairs [region]
  (reduce (fn [acc square]
            (into acc (map (fn [p] [square p])
                           (filter #(not (region %))
                                   (find-virtual-neighbours square)))))
          #{}
          region))

;; 2. for a pair, get all the contiguous pairs and return them as a
;; side, plus the remaining pairs as a separate set

(defn vector-add [avec bvec] (mapv + avec bvec))

(defn get-side [border-pair border-pairs]
  (let [[inside-point outside-point] border-pair
        [dx dy] (apply (partial map -) border-pair)
        left (take-while border-pairs (iterate #(mapv (partial vector-add [dy dx]) %) border-pair))
        right (take-while border-pairs (iterate #(mapv (partial vector-add [(- dy) (- dx)]) %) border-pair))]
    
    (-> #{}
        (into right)
        (into left))))

(defn count-sides [region]
  (loop [acc #{}
         pairs (find-border-pairs region)]
    (if (empty? pairs)
      (count acc)
      (let [side (get-side (first pairs) pairs)]
        (recur (conj acc side) (s/difference pairs side))))))

(defn sided-price [region]
  (* (count region) (count-sides region)))

(defn total-sided-price [regions]
  (reduce + (map sided-price regions)))
