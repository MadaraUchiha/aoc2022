(ns aoc-2022.day01.solution 
  (:require
   [clojure.string :refer [split]]))

(def input (slurp "src/aoc_2022/day01/input.txt"))

(defn- parse-input [input]
	(as-> input $
		(split $ #"\n\n")
		(map #(split % #"\n") $)))

(defn- calculate-calories [cs]
	(->> cs
		(map parse-long)
		(reduce +)))

(defn part1 []
	(->> input
		(parse-input)
		(map calculate-calories)
		(apply max)))

(defn part2 []
	(->> input
		(parse-input)
		(map calculate-calories)
		(sort)
		(reverse)
		(take 3)
		(reduce +)))

(comment
  (parse-input input)
  (time (part1))
  (time (part2)))