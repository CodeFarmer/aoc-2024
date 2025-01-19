(ns aoc-2024.day-15
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def directions
  {\> [ 1  0]
   \< [-1  0]
   \^ [ 0 -1]
   \v [ 0  1]
   })

(def legal-directions (into #{} (keys directions)))

(defn move-thing [tmap pos achar]
  (if (not (legal-directions achar)) tmap
      (let [delta (directions achar)
            pos' (mapv + pos delta)
            tile (aoc/get-tile tmap pos')]
        (cond (= \. tile)
              (-> tmap
                  (aoc/tmap-update pos  \.)
                  (aoc/tmap-update pos' (aoc/get-tile tmap pos)))
              (#{\O \@} tile)
              (let [tmap' (move-thing tmap pos' achar)]
                (if (= tmap' tmap)
                  tmap
                  (move-thing tmap' pos achar)))
              :default tmap))))

(defn move-robot [tmap achar]
  ;; TODO maybe consider maintaining robot state separately
  (let [pos (first (aoc/tmap-find-locations tmap \@))]
    (move-thing tmap pos achar)))

(defn move-robot-string [tmap astr]
  ;; (println "move-robot-string:" astr)
  ;; (aoc/print-tmap tmap)
  (if (empty? astr) tmap
      (recur (move-robot tmap (first astr)) (rest astr))))

(defn gps [tmap]
  (reduce +
          (map (fn [[x y]] (+ x (* 100 y)))
               (aoc/tmap-find-locations tmap \O))))
