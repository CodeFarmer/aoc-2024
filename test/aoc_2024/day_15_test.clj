(ns aoc-2024.day-15-test
  (:require [clojure.test :refer :all]
            [aoc-2024.day-15 :refer :all]
            [aoc-2024.core :as aoc]
            [clojure.string :as str]))


(def example-pmap
  (aoc/pmap "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########"))

(def example-moves
  "<^^>>>vv<v>>v<<")

(deftest move-test
  (is (= example-pmap
           (move-robot example-pmap \<)) "Moving into a solid wall should result in no move")

  ;; FIXME maybe have a call that makes a pmap from a string to DRY this up
  (let [state-2 (aoc/pmap "########
#.@O.O.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########")
        state-2a (aoc/pmap "########
#.@.OO.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########")
        state-3 (aoc/pmap "########
#..@OO.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########")]

    (is (= state-2
           (move-robot example-pmap \^)) "Moving into an empty square should move the robot")
    (is (= state-2
           (move-thing-or-things example-pmap [2 2] \^)) "Moving from the robot's square into an empty square should move the robot")
    (is (= state-2a
           (move-thing-or-things state-2 [3 1] \>)) "Moving from a barrel square into an empty square should move the barrel")

    ;; OK now for the pushing
    (is (= state-3
           (move-robot state-2 \>)) "Moving into a box with an empty square behind it should push the box")))

(def bigger-sample
  (aoc/pmap
   "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########"))

(def bigger-moves
  "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(deftest move-string-test
  (is (= (aoc/pmap "########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########")
         (move-robot-string example-pmap "<^^>>>vv<v>>v<<")))
  (is (= (aoc/pmap "##########
#.O.O.OOO#
#........#
#OO......#
#OO@.....#
#O#.....O#
#O.....OO#
#O.....OO#
#OO....OO#
##########")
         (move-robot-string bigger-sample bigger-moves))))

(deftest gps-test
  (is (= 2028 (gps (move-robot-string example-pmap "<^^>>>vv<v>>v<<"))))
  (is (= 10092 (gps (move-robot-string bigger-sample bigger-moves)))))

(def raw-input
  (str/split
   (slurp "aoc-2024-inputs/input-15.txt")
   #"\n\n"))

(def input-map (aoc/pmap (first raw-input)))
(def input-moves (second raw-input))

;; TODO experiment with different data structures, this seems like
;; tile maps are not the thing efficiency wise. Time to try smaps?
(deftest part-1-test
  (is (= 1360570 (gps (move-robot-string input-map input-moves)))))

;; part 2

(def doubled-sample
  (aoc/pmap
   "####################
##....[]....[]..[]##
##............[]..##
##..[][]....[]..[]##
##....[]@.....[]..##
##[]##....[]......##
##[]....[]....[]..##
##..[][]..[]..[][]##
##........[]......##
####################"))

;; FIXME double-wide should work on pmaps instead
(deftest double-test
  (is (= doubled-sample (pmap-double-wide bigger-sample))))

(deftest move-things-test
  (is (= (aoc/pmap "##############
##......##..##
##..........##
##...[][]@..##
##....[]....##
##..........##
##############")
         (move-robot
          (aoc/pmap "##############
##......##..##
##..........##
##....[][]@.##
##....[]....##
##..........##
##############") \<)))
  (is (= (aoc/pmap "##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############")
         (move-robot
          (aoc/pmap "##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.....@....##
##############") \^))
      "Vertical moves should push adjacent blocks")
  (is (= (aoc/pmap "##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############")
         (move-robot
          (aoc/pmap "##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############") \^))
      "Vertical moves with joined blocks should be blocked if any are blocked"))

(deftest bigger-move-string-test
  (is (= (aoc/pmap "####################
##[].......[].[][]##
##[]...........[].##
##[]........[][][]##
##[]......[]....[]##
##..##......[]....##
##..[]............##
##..@......[].[][]##
##......[][]..[]..##
####################")
         (move-robot-string doubled-sample bigger-moves))))

(deftest doubled-gps-test
  (is (= 9021 (doubled-gps (move-robot-string doubled-sample bigger-moves)))))

(deftest part-2-test
  (let [start (pmap-double-wide input-map)
        finish (move-robot-string start input-moves)]
    (is (= 1381446 (doubled-gps finish)))))
