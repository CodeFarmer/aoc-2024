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

(defn possible-moves [tmap [[x y dir] [pathable cost]]]
  (let [delta (aoc/directions dir)
        turned (mapv (fn [d] [[x y d] [pathable (+ cost 1000)]]) (turns dir))
        pos' (mapv + [x y] delta)]
    (if (= \# (aoc/get-tile tmap pos'))
      turned
      (conj turned [(conj pos' dir) [(conj pathable pos') (inc cost)]]))))

(defn -walk [tmap finish q seen]
  (if (empty? q) [#{} 0] ; technically this is a bug
      (let [[state [pathable cost]] (peek q)
            loc (take 2 state)
            useful? (fn [[s [p c]]] (<= c (second (get seen s [#{} Integer/MAX_VALUE]))))
            finished-states (filter #(= finish (take 2 %)) (keys seen))
            finished-state (first finished-states)]
        (if finished-state ;; wait do we even care?
          (let [[finished-pathable finished-cost] (seen finished-state)]
            (if (> cost finished-cost) ; we are done
              [(reduce into #{}
                       (->> (select-keys seen finished-states)
                            (map second)
                            (map first)))
               finished-cost]
              (recur tmap
                       finish
                       (pop q)
                       (assoc seen state [(into (get-in seen [state 0] #{}) pathable) cost]))))
          (recur tmap
                 finish
                 (into (pop q) (filter useful? (possible-moves tmap [state [pathable cost]])))
                 (assoc seen state [(into (get-in seen [state 0] #{}) pathable) cost]))))))

(defn best-path-score
  ([tmap]
   (best-path-score tmap
                    (finish-square tmap)
                    (conj (pq/priority-queue (fn [[_ [_ c]]] (- c))) [(starting-state tmap) [#{} 0]])
                    {}))
  ([tmap finish q seen]
   (let [[pathable cost] (-walk tmap finish q seen)]
     cost)))

(defn count-best-path-tiles
  ([tmap]
   (count-best-path-tiles tmap
                          (finish-square tmap)
                          (conj (pq/priority-queue (fn [[_ [_ c]]] (- c))) [(starting-state tmap) [#{} 0]])
                          {}))
  ([tmap finish q seen]
   (let [[pathable cost] (-walk tmap finish q seen)]

     ;; (aoc/print-tmap (reduce (fn [a pos] (aoc/tmap-update a pos \O)) tmap pathable))
     (inc (count pathable)))))
