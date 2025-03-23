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

(defn gps [pmap]
  (reduce +
          (map (fn [[x y]] (+ x (* 100 y)))
               (aoc/pmap-find-locations pmap \O))))

; part 2

(defn pmap-double-wide [pmap]
  (-> pmap
      (update :width #(* 2 %))
      (assoc :tiles
             (reduce (fn [acc [x y]]
                       (case (get-in pmap [:tiles [x y]])
                         \O (assoc acc [(* 2 x) y] \[ [(inc (* 2 x)) y] \])
                         \# (assoc acc [(* 2 x) y] \# [(inc (* 2 x)) y] \#)
                         \@ (assoc acc [(* 2 x) y] \@)))
                     {}
                     (keys (:tiles pmap))))))

(def vertical-moves #{\^ \v})
(def partner-offsets {\[ [1 0]
                      \] [-1 0]})

(defn move-thing-or-things [pmap pos achar]
  (if (not (legal-directions achar)) pmap
      (let [delta (directions achar)
            pos' (mapv + pos delta)
            tile (aoc/pmap-get-tile pmap pos')]
        (cond (= \. tile)
              (-> pmap
                  (aoc/pmap-update pos  \.)
                  (aoc/pmap-update pos' (aoc/pmap-get-tile pmap pos)))
              (and (vertical-moves achar) (#{\[ \]} tile))
              (let [pmap' (move-thing-or-things pmap pos' achar)
                    pmap'' (move-thing-or-things pmap' (mapv + pos' (partner-offsets tile)) achar)]
                (if (or (= pmap'' pmap') (= pmap' pmap))
                  pmap
                  (move-thing-or-things pmap'' pos achar)))
              (#{\@ \[ \] \O} tile)
              (let [pmap' (move-thing-or-things pmap pos' achar)]
                (if (= pmap' pmap)
                  pmap
                  (move-thing-or-things pmap' pos achar)))
              :default pmap))))

(defn move-robot [pmap achar]
  ;; TODO maybe consider maintaining robot state separately
  (let [pos (first (aoc/pmap-find-locations pmap \@))]
    (move-thing-or-things pmap pos achar)))

(defn move-robot-string [pmap astr]
  ;; (println "move-robot-string:" astr)
  ;; (aoc/print-tmap tmap)
  (if (empty? astr) pmap
      (recur (move-robot pmap (first astr)) (rest astr))))

(defn doubled-gps [pmap]
  ;; DRY up
  (reduce +
          (map (fn [[x y]] (+ x (* 100 y)))
               (aoc/pmap-find-locations pmap \[))))
