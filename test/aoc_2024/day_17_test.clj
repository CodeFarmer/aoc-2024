(ns aoc-2024.day-17-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-17 :refer :all]
            [clojure.string :as str]))

(deftest combo-test
  (is (= [0 1 2 3] (map (partial combo {}) [0 1 2 3]))
      "Combo operands 0 to 3 are literals 0..3")
  (is (= 1 (combo {:A 1 :B 2 :C 3} 4))
      "Combo operand 4 is the contents of the A register")
  (is (= 2 (combo {:A 1 :B 2 :C 3} 5))
      "Combo operand 5 is the contents of the B register")
  (is (= 3 (combo {:A 1 :B 2 :C 3} 6))
      "Combo operand 6 is the contents of the C register")
  (is (nil? (combo {:A 1 :B 2 :C 3} 7))
      "Combo operand 7 is not valid"))

(deftest adv-test
  (is (= {:A 3 :pc 2} (adv {:A 12 :pc 0} 2))
      "adv divides A by two to the power of the combo operand when literal")
  (is (= {:A 2 :B 3 :pc 2}
         (adv {:A 16 :B 3 :pc 0} 5))
      "adv divides A by two to the power of the combo operand when combined")
  (is (= {:A 3 :pc 4} (adv {:A 7 :pc 2} 1))
      "adv divides A by two to the power of the combo operand truncated to an integer")
  (is (= {:A 1 :pc 4} (adv {:A 9 :pc 2} 3))
      "adv divides A by two to the power of the combo operand truncated to an integer"))

(deftest bxl-test
  (is (= {:B 1 :pc 2} (bxl {:B 1 :pc 0} 0))
      "bxl performs the bitwise XOR of register B and the instruction's literal operand")
  (is (= {:B 4 :pc 4} (bxl {:B 2 :pc 2} 6))
      "bxl performs the bitwise XOR of register B and the instruction's literal operand"))

(deftest bst-test
  (is (= {:A 11 :B 3 :pc 2} (bst {:A 11 :B 0 :pc 0} 4))
      "bst calculates the value of its combo operand, modulo 8, and stores it in the B register"))

(deftest jnz-test
  (is (= {:A 0 :pc 14} (jnz {:A 0 :pc 12} 0))
      "jnz does not modify program pointer (other than incrementing) when register A is zero")
  (is (= {:A 1 :pc 6} (jnz {:A 1 :pc 12} 6))
      "jnz modifies program pointer to literal operand when register A is nonzero"))

(deftest bxc-test
  (is (= {:B 4 :C 5 :pc 2} (bxc {:B 1 :C 5 :pc 0} nil))
      "bxc performns bitwise XOR of B and C registers and stores them in B"))

(deftest out-test
  (is (= {:A 11 :pc 2 :output [3]} (out {:A 11 :pc 0 :output []} 4))
      "The out instruction outputs the value of its combo operand modulo 8"))


(deftest bdv-test
  "bdv works like adv, but on the B register"
  (is (= {:A 12 :B 3 :pc 2} (bdv {:A 12 :B 0 :pc 0} 2))))

(deftest cdv-test
  "cdv works like adv, but on the B register"
  (is (= {:A 12 :C 3 :pc 2} (cdv {:A 12 :C 0 :pc 0} 2))))

(def sample-data
  {:A 729
   :B 0
   :C 0
   :pc 0
   :output []
   :program [0 1
             5 4
             3 0]})

(deftest program-running-test
  (is (= "4,6,3,5,6,3,5,2,1,0"
         (run-program sample-data))))

(def input-data
  {:A 24847151
   :B 0
   :C 0
   :pc 0
   :output []
   :program [2 4 ;; bst A ; A mod 8   -> B (last 3 bits of A)
             1 5 ;; bxl 1 ; B xor 1   -> B
             7 5 ;; cdv B ; A div 2^B -> C (A << B)
             1 6 ;; bxl 6 ; B xor 6   -> B
             0 3 ;; adv 3 ; A div 8   -> A
             4 0 ;; bxc _ ; B xor C   -> B
             5 5 ;; out B ; B mod 8
             3 0 ;; jnz 0
             ]})

(deftest part-1-test
  (is (= "7,3,1,3,6,3,6,0,2"
         (run-program input-data))))

;; part 2

(def quine-data
  {:A 2024
   :B 0
   :C 0
   :pc 0
   :output []
   :program [0 3 ;; adv 3
             5 4 ;; out A
             3 0 ;; jnz 0
             ]})

(deftest quine-test
  (is (quine? (assoc quine-data :A 117440))))

(deftest shortcut-quine-test
  (is (shortcut-quine? (assoc quine-data :A 117440)))
  (is (not (shortcut-quine? input-data))))

(comment 
  (deftest find-quinable-A-test
    (is (= 0 (find-quinable-A input-data)))))

;; we know that:

;; divided by 8 16 times to reach zero:
;; min 8^15 (35184372088832)
;; max 8^16 (281474976710656)

;; lower 3 bits of A -> B
;; B <- (A mod 8)

;; 3-bit numbers
;; (xor (xor x 1) 6) = 7 - x
