(ns aoc-2024.day-9-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-9 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))

(def sample-data
  "2333133121414131402")

(def sample-layout
  "00...111...2...333.44.5555.6666.777.888899")

(def sample-fs
  [{:id 0 :size 2}
   {:empty :empty :size 3}
   {:id 1 :size 3}
   {:empty :empty :size 3}
   {:id 2 :size 1}
   {:empty :empty :size 3}
   {:id 3 :size 3}
   {:empty :empty :size 1}
   {:id 4 :size 2}
   {:empty :empty :size 1}
   {:id 5 :size 4}
   {:empty :empty :size 1}
   {:id 6 :size 4}
   {:empty :empty :size 1}
   {:id 7 :size 3}
   {:empty :empty :size 1}
   {:id 8 :size 4}
   {:id 9 :size 2}])

(deftest parsing-test
  (is (= sample-fs (parse-fs sample-data))))

(deftest fs-string-test
  (is (= sample-layout (fs-string sample-fs))))

(deftest crush-empty-blocks-test
  (let [fs [{:id 0 :size 1}
            {:empty :empty :size 0}
            {:id 1 :size 1}
            {:empty :empty :size 2}
            {:empty :empty :size 3}
            {:empty :empty :size 1}
            {:id 2 :size 1}]]
    (is (= [{:id 0 :size 1}
            {:id 1 :size 1}
            {:empty :empty :size 6}
            {:id 2 :size 1}]
           (crush-empty-blocks fs))
        "consecutive empty blocks should be joined, and zero-length empty blocks removed")))

(deftest compaction-step-test
  (let [fs (parse-fs "12345")
        c1 (compact-step fs)
        c2 (compact-step c1)]
    (is (= "0..111....22222" (fs-string fs)) "sanity check")
    (is (= "022111....222.."
           (fs-string c1)))
    (is (= "022111222......"
           (fs-string c2)))))

(deftest total-compaction-test
  (let [fs (parse-fs "12345")]
    (is (= "022111222......"
           (fs-string (compact-totally fs)))))
  (let [fs (parse-fs sample-data)]
    (is (= "0099811188827773336446555566.............."
           (fs-string (compact-totally fs))))))

(deftest checksum-test
  (is (= 1928 (checksum (compact-totally (parse-fs sample-data))))))

(def input-data
  (str/trim (slurp "aoc-2024-inputs/input-9.txt")))

(deftest part-1-test
  (let [fs (parse-fs input-data)]
    (is (= 6258319840548 (checksum (compact-totally fs))))))

;; part 2

(deftest compact-nofrag-test
  (let [fs (parse-fs sample-data)]
    (is (= "00992111777.44.333....5555.6666.....8888.."
           (fs-string (compact-nofrag fs))))))

(deftest part-2-test
  (let [fs (parse-fs input-data)]
      (is (= 6286182965311 (checksum (compact-nofrag fs))))))

;; parts 1 and 2 together take about 1:30 on my i5 desktop, and 3:30
;; on my old Thinkpad. This is very much not OK and I'll come back to
;; it.
