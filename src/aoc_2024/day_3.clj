(ns aoc-2024.day-3
  (:require [aoc-2024.core :refer :all]
            [clojure.string :as str]))



(defn get-multiplies [astr]
  (let [ms (re-seq (re-pattern "mul\\((\\d{1,3}),(\\d{1,3})\\)") astr)]
    (if (empty? ms)
      [[]]
      (->> ms
           (map rest)
           (map intify-seq)))))
