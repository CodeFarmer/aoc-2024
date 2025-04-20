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

(def backtracks
  {:left  :right
   :right :left
   :down  :up
   :up    :down})

(defn possible-moves [tmap [[x y dir] [pathable cost]]]
  (let [delta (aoc/directions dir)
        turned (mapv (fn [d] [[x y d] [pathable (+ cost 1000)]]) (turns dir))
        pos' (mapv + [x y] delta)]
    (if (= \# (aoc/get-tile tmap pos'))
      turned
      (conj turned [(conj pos' dir) [(conj pathable pos') (inc cost)]]))))

(defn -walk
  ;; TODO why is it taking me 75000 iterations to see 4000 squares?
  ([tmap finish q seen]
   (-walk tmap finish q seen {} 0 (System/currentTimeMillis)))
  
  ([tmap finish q seen finished-states i start-time]

   ;; DEBUG
   (comment 
     (if (zero? (mod i 1000))
       (let [seen-squares (into #{} (map #(take 2 %) (keys seen)))
             q-count (count q)]
         (aoc/print-tmap (reduce (fn [a p] (aoc/tmap-update a p \O))
                                 tmap
                                 seen-squares))
         (println "i:" i "q-count:" q-count "seen-squares:" (count seen-squares) "~cost:" (second (second (peek q))) "t:" (int (/ (- (System/currentTimeMillis) start-time) 1000))))))

   (if (empty? q)
     [#{} 0] ;; probably this is a bug
     (let [[state [pathable cost]] (peek q)
           loc (take 2 state)
           finished-states' (if (= finish loc)
                              (assoc finished-states state [(into (get-in finished-states [state 0] #{}) pathable) cost])
                              finished-states)
           useful? (fn [[s [p c]]]
                     (<= c (second (get seen s [#{} Integer/MAX_VALUE]))))]
       (if (not (empty? finished-states))
         (let [finished-state (first (keys finished-states))
               [finished-pathable finished-cost] (finished-states finished-state)]
           (if (> cost finished-cost) ;; we are done
             [(reduce into #{} ;; merge all the pathables from the finished states
                      (->> finished-states'
                           (map second)
                           (map first)))
              finished-cost]
             (recur tmap
                    finish
                    (pop q) ;; stop queueing new things, they can't possibly be shortest finishes
                    (assoc seen state [(into (get-in seen [state 0] #{}) pathable) cost])
                    finished-states'
                    (inc i)
                    start-time)))
         (recur tmap
                finish
                (into (pop q) (if (useful? [state [pathable cost]])
                                (filter useful? (possible-moves tmap [state [pathable cost]]))
                                []))
                (assoc seen state [(into (get-in seen [state 0] #{}) pathable) cost])
                finished-states'
                (inc i)
                start-time))))))

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

     (comment
       (aoc/print-tmap (reduce (fn [a pos] (aoc/tmap-update a pos \O)) tmap pathable)))
     (inc (count pathable)))))
