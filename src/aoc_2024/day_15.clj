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

(defn gps [tmap]
  (reduce +
          (map (fn [[x y]] (+ x (* 100 y)))
               (aoc/tmap-find-locations tmap \O))))

; part 2

(defn double-wide [tmap]
  (into []
        (map (fn [astr]
               (-> astr
                   (str/replace #"O" "[]")
                   (str/replace #"#" "##")
                   (str/replace #"\." "..")
                   (str/replace #"@" "@.")))
             tmap)))

(def vertical-moves #{\^ \v})
(def partner-offsets {\[ [1 0]
                      \] [-1 0]})

(defn move-thing-or-things [tmap pos achar]
  (if (not (legal-directions achar)) tmap
      (let [delta (directions achar)
            pos' (mapv + pos delta)
            tile (aoc/get-tile tmap pos')]
        (cond (= \. tile)
              (-> tmap
                  (aoc/tmap-update pos  \.)
                  (aoc/tmap-update pos' (aoc/get-tile tmap pos)))
              (and (vertical-moves achar) (#{\[ \]} tile))
              (let [tmap' (move-thing-or-things tmap pos' achar)
                    tmap'' (move-thing-or-things tmap' (mapv + pos' (partner-offsets tile)) achar)]
                (if (or (= tmap'' tmap') (= tmap' tmap))
                  tmap
                  (move-thing-or-things tmap'' pos achar)))
              (#{\@ \[ \] \O} tile)
              (let [tmap' (move-thing-or-things tmap pos' achar)]
                (if (= tmap' tmap)
                  tmap
                  (move-thing-or-things tmap' pos achar)))
              :default tmap))))

(defn move-robot [tmap achar]
  ;; TODO maybe consider maintaining robot state separately
  (let [pos (first (aoc/tmap-find-locations tmap \@))]
    (move-thing-or-things tmap pos achar)))

(defn move-robot-string [tmap astr]
  ;; (println "move-robot-string:" astr)
  ;; (aoc/print-tmap tmap)
  (if (empty? astr) tmap
      (recur (move-robot tmap (first astr)) (rest astr))))

(defn doubled-gps [tmap]
  ;; DRY up
  (reduce +
          (map (fn [[x y]] (+ x (* 100 y)))
               (aoc/tmap-find-locations tmap \[))))
