(ns aoc-2022.day03.solution 
  (:require
   [clojure.set :refer [intersection]]
   [clojure.string :refer [split]]))

(def input (slurp "src/aoc_2022/day03/input.txt"))

(defn priority [char]
	(let [uppercased (Character/toUpperCase char)
		  uppercase? (= uppercased char)]
		(+
			(- (int uppercased) 64)
			(if uppercase? 26 0))))

(defn parse-line [line]
	(let [cutoff       (/ (count line) 2)
		  [left right] (split-at cutoff line)]
		[(into #{} (seq left))
		 (into #{} (seq right))]))

(defn parse-elf-group [group]
	(map #(into #{} (seq %)) group))

(defn parse-input1 [input]
	(as-> input $
		(split $ #"\n")
		(map parse-line $)))

(defn parse-input2 [input]
	(as-> input $
		(split $ #"\n")
		(partition 3 $)
		(map parse-elf-group $)))

(defn find-common [groups]
	(first (apply intersection groups)))

(defn solve-part [input parse-fn]
	(->> input
		(parse-fn)
		(map find-common)
		(map priority)
		(reduce +)))

(defn part-1 [input]
	(solve-part input parse-input1))

(defn part-2 [input]
    (solve-part input parse-input2))

(comment
  (parse-input1 input)
  (priority \Z)
  (first #{\a})
  (part-1 input)
  (part-2 input))