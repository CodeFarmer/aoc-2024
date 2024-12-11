(ns aoc-2024.day-11
  (:require [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(defn blink [i]
  (let [s (str i)
        c (count s)]
    (cond (= 0 i) [(inc i)]
          (even? c) [(parse-long (subs s 0 (/ c 2))) (parse-long (subs s (/ c 2)))]
          :default [(* i 2024)])))

(defn blink-stones [aseq]
  (apply concat (map blink aseq)))

(defn count-after-blinks [aseq n]
  (count (nth (iterate blink-stones aseq) n)))

;; part 2

(def iterated-blink-count
  (memoize ;; this can probably be improved, it's doubtful the whole cache is useful
   (fn [n i]
     (if (zero? n)
       1
       (reduce + (bigint 0)
               (map (partial iterated-blink-count (dec n))
                    (blink i)))))))

(defn iterated-blink-count-seq
  [n aseq]
  (reduce + (bigint 0) (map (partial iterated-blink-count n) aseq)))
