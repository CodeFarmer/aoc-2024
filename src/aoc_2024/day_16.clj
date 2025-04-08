(ns aoc-2024.day-16
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]
            [shams.priority-queue :as pq]))

(defn starting-state [tmap]
  (conj (first (aoc/tmap-find-locations tmap \S))
        :right))

(defn finish-square [tmap]
  (first (aoc/tmap-find-locations tmap \E)))


(def turns
  {:left  [:down :up]
   :up    [:left :right]
   :right [:up :down]
   :down  [:right :left]})

(defn possible-moves [tmap [[x y dir] cost]]
  (let [delta (aoc/directions dir)
        turned (mapv (fn [d] [[x y d] (+ cost 1000)]) (turns dir))
        pos' (mapv + [x y] delta)]
    (if (= \# (aoc/get-tile tmap pos'))
      turned
      (conj turned [(conj pos' dir) (inc cost)]))))

(defn -walk [tmap finish q seen]
  (if (empty? q) 0
       (let [[state cost] (peek q)
             loc (take 2 state)
             useful? (fn [[s c]] (< c (get seen s Integer/MAX_VALUE)))]
         (if (= finish loc)
           cost
           (recur tmap
                  finish
                  (into (pop q) (filter useful? (possible-moves tmap [state cost])))
                  (assoc seen state cost))))))

(defn best-path-score
  ([tmap]
   (best-path-score tmap
                    (finish-square tmap)
                    (conj (pq/priority-queue #(- (second %))) [(starting-state tmap) 0])
                    {}))
  ([tmap finish q seen]
   (-walk tmap finish q seen)))

(defn count-best-path-tiles [tmap]
  0)
