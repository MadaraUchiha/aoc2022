(ns aoc-2022.day02.solution 
  (:require
   [clojure.string :refer [split]]))

(def sample-input 
	"A Y
B X
C Z")

(def input (slurp "src/aoc_2022/day02/input.txt"))

(def part1-their-hand {"A" :rock "B" :paper "C" :scissors})
(def part1-my-hand {"X" :rock "Y" :paper "Z" :scissors})

(def part2-result-table {"X" :lose "Y" :draw "Z" :win})

(def result-score-table {:lose 0 :draw 3 :win 6})
(def hand-score-table {:rock 1 :paper 2 :scissors 3})

(defn play-round 
	"Play a single round and return the result"
	[my-hand their-hand]
	(case [my-hand their-hand]
		[:rock :paper]     :lose
		[:paper :scissors] :lose
		[:scissors :rock]  :lose
		[:rock :scissors]  :win
		[:paper :rock]     :win
		[:scissors :paper] :win
		                   :draw))

(defn score
	[[my-hand their-hand]]
	(+
		(result-score-table (play-round my-hand their-hand))
		(hand-score-table my-hand)))

(defn deduce-hand [their-hand result]
	(case [their-hand result]
		[:rock :win]      :paper
		[:rock :lose]     :scissors
		[:paper :win]     :scissors
		[:paper :lose]    :rock
		[:scissors :win]  :rock
		[:scissors :lose] :paper
						  their-hand))

(defn parse-input1 [input]
	(as-> input $
		(split $ #"\n")
		(map #(split % #" ") $)
		(mapv (fn [[theirs mine]] [(part1-my-hand mine) (part1-their-hand theirs)]) $)))

(defn parse-input2 [input]
	(as-> input $
		(split $ #"\n")
		(map #(split % #" ") $)
		(mapv (fn [[theirs result]] [(part1-their-hand theirs) (part2-result-table result)]) $)))

(defn part-1 [input]
	(->> input
		(parse-input1)
		(map score)
		(reduce +)))

(defn part-2 [input]
	(let [parsed (parse-input2 input)
		  their-hands (map first parsed)
		  my-hands (map #(apply deduce-hand %) parsed)]
	    (->> (map vector my-hands their-hands)
	    	(map score)
	    	(reduce +))))
	    

(comment
	(parse-input1 sample-input)
	(part-1 input)
	(part-2 input)
	(score [:rock :paper])
	(map vector [1 2 3] [2 3 5])
	)