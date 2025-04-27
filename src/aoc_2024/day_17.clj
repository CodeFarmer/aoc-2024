(ns aoc-2024.day-17
  (:require [aoc-2024.core :as aoc]
            [clojure.math :as m]
            [clojure.string :as str]))

(defn combo [machine operand]
  (cond (<= 0 operand 3) operand
        (= 4 operand) (:A machine)
        (= 5 operand) (:B machine)
        (= 6 operand) (:C machine)
        :default nil))

(defn increment-pc [machine]
  (update machine :pc (partial + 2)))

(defn -dv [machine operand reg]
      (-> machine
          (assoc reg (bit-shift-right (machine :A) (combo machine operand)))
          (increment-pc)))

(defn adv [machine operand]
  (-dv machine operand :A))

(defn bdv [machine operand]
  (-dv machine operand :B))

(defn cdv [machine operand]
  (-dv machine operand :C))

(defn bxl [machine operand]
  (-> machine 
      (update :B #(bit-xor % operand))
      (increment-pc)))

(defn bst [machine operand]
  (-> machine
      (assoc :B (mod (combo machine operand) 8))
      (increment-pc)))

(defn jnz [machine operand]
  (if (zero? (:A machine))
    (update machine :pc (partial + 2))
    (assoc machine :pc operand)))

(defn bxc [machine _]
  (-> machine
      (update :B #(bit-xor % (:C machine)))
      (increment-pc)))

(defn out [machine operand]
  (-> machine
      (update :output #(conj % (mod (combo machine operand) 8)))
      (increment-pc)))

(def opcodes
  [adv
   bxl
   bst
   jnz
   bxc
   out
   bdv
   cdv])

(defn run-program [machine]
  (let [pc (:pc machine)
        program (:program machine)]
    (if (>= pc (count program))
      (str/join "," (:output machine))
      (recur
       ((opcodes (program pc))
        machine
        (program (inc pc)))))))

;; part 2

(defn quine? [machine]
  (let [final-state (run-program machine)]
    (= (:program final-state)
       (:output final-state))))

(defn shortcut-quine? [machine]
  
  (comment
    (println machine))
  
  (let [pc (:pc machine)
        program (:program machine)]

    (if (>= pc (count program))
      (= program (:output machine))
      (let [op (opcodes (program pc))
            machine' (op
                      machine
                      (program (inc pc)))
            output' (:output machine')]
        (if (and (= out op)
                 (not (= output' (take (count output') program))))
          false
          (recur machine'))
        ))))

;; brute force, this is not going to work on its own.

(defn find-quinable-A
  ([machine]
   (find-quinable-A machine 0))
  ([machine a]
   (if (zero? (mod a 1000000))
     (println a ":" (run-program (assoc machine :A a))))
   (if (shortcut-quine? (assoc machine :A a))
     a
     (recur machine (inc a)))))
