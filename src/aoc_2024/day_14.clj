(ns aoc-2024.day-14
  (:require [aoc-2024.core :as aoc]
            [clojure.math :as math]
            [clojure.string :as str]))

;; p=0,4 v=3,-3

(defn parse-robot [astr]
  (->> astr
       (re-matches #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
       (rest)
       (map parse-long)
       (partition 2)))

(defn tick-robot
  ([[[x y] [dx dy]]]
   [[(+ x dx) (+ y dy)] [dx dy]])
  ([width height [[x y] [dx dy]]]
   [[(mod (+ x dx) width) (mod (+ y dy) height)] [dx dy]]))

;; return a map of occupied squares to the number of robots in each
;; square
;; It turns out this is entirely unnecessary
(defn square-robot-counts [robots]
  (reduce (fn [acc [p v]]
            (assoc acc p (inc (get acc p 0))))
          {}
          robots))


(defn robot-quadrant-counts [width height robots]
  (let [middle-row (math/floor (/ height 2))
        middle-col (math/floor (/ width 2))
        top-left? (fn [[[x y] v]] (and (< x middle-col)
                                       (< y middle-row)))
        top-right? (fn [[[x y] v]] (and (> x middle-col)
                                        (< y middle-row)))
        bottom-left? (fn [[[x y] v]] (and (< x middle-col)
                                          (> y middle-row)))
        bottom-right? (fn [[[x y] v]] (and (> x middle-col)
                                           (> y middle-row)))]
    [(count (filter top-left? robots))
     (count (filter top-right? robots))
     (count (filter bottom-left? robots))
     (count (filter bottom-right? robots))]))

(defn safety-factor [width height seconds robots]
  (apply * (robot-quadrant-counts width height (nth (iterate #(map (partial tick-robot width height) %) robots) seconds))))

;; part 2

(defn empty-tmap [width height]
  (into [] (repeat height (apply str (repeat width \.)))))

(defn draw-robots-as-tmap [width height robots]
  (reduce (fn [tmap [[x y] v]] (aoc/tmap-update tmap [x y] \#))
          (empty-tmap width height)
          robots))

;; There is a recurring pattern of converging robots every 101 (width)
;; turns, assumption is that it will eventually show the tree
(defn find-christmas-tree-manually [width height n next-n robots]
  (if (= 0 (mod n 100))
    (do (aoc/print-tmap (draw-robots-as-tmap width height robots))
        (println n)))
  (if (= n next-n)
    (do
      (aoc/print-tmap (draw-robots-as-tmap width height robots))
      (print n ": is this a Christmas tree? (n): ") (flush)
      (flush)
      (let [answer (read-line)]
        (if (= "y" answer)
          n
          (recur width height (inc n) (+ n width) (map (partial tick-robot width height) robots)))))
    (recur width height (inc n) next-n (map (partial tick-robot width height) robots))))


(defn has-vertical-run?
  ([n robots]
   (let [robot-positions (into #{} (map first robots))]
     (some #(has-vertical-run? % n robot-positions) robot-positions)))
  ([robot-pos n robot-positions]
   (if (zero? n)
     true
     (let [next-robot (mapv + robot-pos [0 1])]
       (if (robot-positions next-robot)
         (recur next-robot (dec n) robot-positions))))))

(defn find-christmas-tree-boringly [width height n robots]
  (if (has-vertical-run? 10 robots)
    n
    (recur width height (inc n) (map (partial tick-robot width height) robots))))
