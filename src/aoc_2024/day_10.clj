(ns aoc-2024.day-10
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(defn find-next-steps [tmap [x y]]
  (let [val (int (aoc/get-tile tmap [x y]))]
    (filter #(= (inc val) (int (aoc/get-tile tmap %)))
           (aoc/tmap-find-neighbours [x y] tmap))))

(defn reachable-goals
  ([tmap [x y]]
   (reachable-goals #{}
                    (into clojure.lang.PersistentQueue/EMPTY (find-next-steps tmap [x y]))
                    tmap))
  ([goal-set q tmap]
   (comment (println "reachable-goals" goal-set (into [] q)))
   (if (empty? q)
     goal-set 
     (let [b (peek q)]
       (if (= \9 (aoc/get-tile tmap b))
         (recur (conj goal-set b) (pop q) tmap)
         (recur goal-set (into (pop q) (find-next-steps tmap b)) tmap))))))

(defn score [tmap]
  (->> (aoc/tmap-find-locations tmap \0)
       (map (partial reachable-goals tmap))
       (map count)
       (reduce +)))

(defn rating [tmap]
  (->> (aoc/tmap-find-locations tmap \0)
       (map #(reachable-goals
                      []
                      (into clojure.lang.PersistentQueue/EMPTY (find-next-steps tmap %))
                      tmap))
       (map count)
       (reduce +)))
