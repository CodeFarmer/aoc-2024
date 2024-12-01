(ns aoc-2024.core)

;; AOC utility functions, carried over from the 2023 version

(defn lines-as-vector [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into [] (line-seq rdr))))

(defn intify-seq
  "Transform a sequence of strings into a sequence of integers"
  [aseq]
  (map #(Integer/parseInt %) aseq))

;; turn a map inside out (v -> k)
(defn minverse [amap]
  (reduce (fn [a [k v]] (assoc a v k)) {} amap))

;; functions dealing with 2D maps expressed as vectors of
;; strings (rows)

(defn tmap-height [tmap]
  (count tmap))

(defn tmap-width [tmap]
  (count (first tmap)))

(def neighbour-deltas
  [[-1 0] [0 -1] [1 0] [0 1]])

;; FIXME switch args around
(defn tmap-find-neighbours [point tmap]
  (let [width (tmap-width tmap)
        height (tmap-height tmap)]
    (->> neighbour-deltas
         (map #(map + point %))
         (filter (fn [[x y]] (and (>= x 0) (< x width)
                                  (>= y 0) (< y height)))))))

(defn tmap-rotate-right
  "Given a map expressed as a vector of strings (each a single line of the map), rotate it 90 degrees clockwise"
  ([avec] (tmap-rotate-right avec []))
  ([avec acc]
   (if (empty? (first avec))
     acc
     (recur (map rest avec) (conj acc (apply str (reverse (map first avec))))))))

(defn tmap-rotate-left
  "Given a map expressed as a vector of strings (each a single line of the map), rotate it 90 degrees clockwise"
  ([avec] (tmap-rotate-left avec '()))
  ([avec acc]
   (if (empty? (first avec))
     (into [] acc)
     (recur (map rest avec) (conj acc (apply str (map first avec)))))))

(defn get-tile
  "Given a map expressed as a vector of strings, find the tile character ar [x y]"
  [tile-map [x y]]
  (get-in tile-map [y x]))

(def directions
  {:right [ 1  0]
   :left  [-1  0]
   :up    [ 0 -1]
   :down  [ 0  1]
   })

(defn tmap-update [tmap [x y] c]
  (assoc tmap y
         (let [row (get tmap y)]
           (str (.substring row 0 x) c (.substring row (inc x))))))

(defn print-tmap [tmap]
  (doseq [r tmap] (println r)))

;; finding cycles in sequences

;; assumptions: the sequence is deterministic in that once the first
;; member of a cycle appears, the rest of the cycle is certain to
;; follow it
;; (for example, the output of core.iterate)

(defn find-cycle
  "In a deterministic stateless sequence (for example the output of core.iterate), find the first index of the beginning of a cycle, and return the cycle contents"
  ([aseq]
   (find-cycle #{} aseq aseq))
  ([acc aseq looking-seq]
   (if (empty? looking-seq) nil
       (let [x (first looking-seq)]
         (if (acc x) ;; just finished the cycle  
           (let [start (drop-while #(not (= x %)) aseq)
                 tail (take-while #(not (= x %)) (rest start))]
             (conj tail x))
           (recur (conj acc x) aseq (rest looking-seq)))
         ))))

(defn nth-with-cycles
  "Given a sequence with a cycle in it, guess the nth element of the sequence"
  [aseq n]
  (let [cyc (find-cycle aseq)
        cl (count cyc)
        x (first cyc)
        prelude (take-while #(not (= x %)) aseq)
        pl (count prelude)]
    (if (< n pl)
      (nth prelude n)
      (nth cyc (mod (- n pl) cl)))))

(defn ctoi [achar]
  (- (int achar) 48))
