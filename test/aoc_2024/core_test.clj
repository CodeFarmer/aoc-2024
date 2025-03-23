(ns aoc-2024.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc-2024.core :refer :all]))

(deftest minverse-test
  (is (= {} (minverse {})))
  (is (= {:b :a} (minverse {:a :b})))
  (is (= {[1 2] :a
          [2 3] :b} (minverse {:a [1 2] :b [2 3]}))))


(deftest map-rotate-test
  (testing "rotation clockwise"
    (testing "tile map"
      (is (= [] (tmap-rotate-right [])))
      (is (= ["1"] (tmap-rotate-right ["1"])))
      (is (= ["1"
              "2"] (tmap-rotate-right ["12"])))
      (is (= ["21" (tmap-rotate-right ["1"
                                       "2"])]))
      
      (is (= ["741"
              "852"
              "963"]
             (tmap-rotate-right ["123"
                                 "456"
                                 "789"])))))
  (testing "rotation counterclockwise"
    (testing "tile map"
      (is (= ["369"
              "258"
              "147"]
             (tmap-rotate-left ["123"
                                "456"
                                "789"]))))))

(def simple-city
  ["1133"
   "3133"
   "3111"])

(deftest map-update-test
  (testing "tile map"
    (is (= ["1133"
            "31K3"
            "3111"]
           (tmap-update simple-city [2 1] \K)))))

(deftest neighbour-finding-test
  (testing "tile map"
    (is (= [[1 0] [0 1]]
           (tmap-find-neighbours [0 0] simple-city)))))

(deftest find-locations-test
  (let [sample-data
        (into []
              (str/split
               "..X...
.SAMX.
.A..A.
XMAS.S
.X...." #"\n"))]
    (testing "tile map"
      (is (= [[2 0] [4 1] [0 3] [1 4]]
             (tmap-find-locations sample-data \X))))))

(deftest pmap-and-tmap-test
  (is (= simple-city
         (pmap-to-tmap 
          (tmap-to-pmap simple-city)))))

(deftest cycle-finding-test
  (is (= nil  (find-cycle [1 2 3 4 5]))
      "No cycle found")
  (is (= [1] (find-cycle [1 1 1 1])))

  (is (= [1 2] (find-cycle [1 2 1 2]))
      "Cycles at the beginning should be found")
  (is (= [2 3 4] (find-cycle [1 2 3 4 2 3 4 2 3 4]))
      "Cycles not at the beginning should be found"))

(deftest nth-with-cycles-test
  (is (= 1 (nth-with-cycles [1 2 3 2 3] 0)))
  (is (= 2 (nth-with-cycles [1 2 3 2 3] 3)))
  (is (= 2 (nth-with-cycles [1 2 3 2 3] 15))
      "n can be off the end of the original seq")
  (is (= 2 (nth-with-cycles [1 2 3 2 3] 1000000015))
      "n can be really large and not cause problems"))

;; other

(deftest ctoi-test
  (is (= 0 (ctoi \0)))
  (is (= 1 (ctoi \1)))
  (is (= 9 (ctoi \9))))

(deftest string-to-ints-test
  (is (= [4 3] (string-to-ints "4   3"))))
