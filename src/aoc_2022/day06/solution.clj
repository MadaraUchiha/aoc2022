(ns aoc-2022.day06.solution)

(def input (slurp "src/aoc_2022/day06/input.txt"))

(defn find-first-repeat [str window-size]
	(->> str
		(partition window-size 1) 								;; sliding window of size window-size
		(map vector (range))      								;; pair with index
		(filter #(= window-size (count (into #{} (second %))))) ;; find those who, when converted to a character Set, remain the same size
		(first)													;; get the first
		(first)													;; get the index
		(+ window-size)))										;; compensate for partition starting the count only after a full window

(defn part-1 [input]
	(find-first-repeat input 4))

(defn part-2 [input]
	(find-first-repeat input 14))

(comment
	(part-1 input)
	(part-2 input))