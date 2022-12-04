(ns aoc-2022.day04.solution 
  (:require
   [clojure.string :refer [split]]
   [com.rpl.specter :refer [transform ALL]]))

(def input (slurp "src/aoc_2022/day04/input.txt"))

(defn parse-line [line]
  (->> (split line #",")
       (map #(split % #"-"))))

(defn parse-input [input]
  (->> (split input #"\n")
       (map parse-line)
       (transform [ALL ALL ALL] parse-long)))

(defn fully-contained? [[[x1 y1] [x2 y2]]]
  (or (and (<= x1 x2)
           (>= y1 y2))
      (and (>= x1 x2)
           (<= y1 y2))))

(defn overlaps? [[[x1 y1] [x2 y2]]]
  (or (and (<= x1 y2)
           (>= y1 x2))
      (and (<= x2 y1)
           (>= y2 x1))))


(defn part-1 [input]
  (->> input
       (parse-input)
       (filter fully-contained?)
       (count)))

(defn part-2 [input]
  (->> input
       (parse-input)
       (filter overlaps?)
       (count)))

(comment
  (parse-input input)
  (fully-contained? '([0 4] [1 4]))
  (overlaps? '([1 3] [2 5]))
  (part-1 input)
  (part-2 input))