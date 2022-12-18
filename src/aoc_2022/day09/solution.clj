(ns aoc-2022.day09.solution 
  (:require
   [aoc-2022.helpers :refer [v+ v-]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))

(def input (slurp "src/aoc_2022/day09/input.txt"))
(def sample-input (slurp "src/aoc_2022/day09/sample.txt"))

(comment "parsed instructions"
  '({:dir [0 1] :steps 5}
    {:dir [1 0] :steps 3}
    ...))

(comment "old structure"
  '({:head [0 0] :tail [0 0]}
    {:head [0 1] :tail [0 0]}
    {:head [0 2] :tail [0 1]}))

(comment "new structure"
  '([[0 0] [0 0] [0 0] [0 0]]
    [[0 1] [0 0] [0 0] [0 0]]))

(def char-to-dir 
  {"U" [0 1]
   "D" [0 -1]
   "L" [-1 0]
   "R" [1 0]})

(defn parse-instruction [inst]
  (let [[dir steps] (str/split inst #" ")]
    {:dir (char-to-dir dir) :steps (parse-long steps)}))
  
(defn expand-steps [{:keys [dir steps]}]
  (repeat steps dir))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (map parse-instruction)
       (map expand-steps)
       (apply concat)))

;; Because I'm a lazy fucker
(def towards-zero 
  {2  1
   1  0
   0  0
   -1 0
   -2 -1})

(defn movable? [relative-vector]
  (->> relative-vector
    (map abs)
    (some #(< 1 %))))

(defn move-towards [target subject]
  (let [relative (v- subject target)
        relatively-moved (map towards-zero relative)
        absolutely-moved (v+ target relatively-moved)]
    absolutely-moved))

(defn pull-tail [new-head old-tail]
  (let [relative (v- old-tail new-head)]
    (if (movable? relative)
      (move-towards new-head old-tail)
      old-tail))) 

(defn move-rope [rope dv]
  (let [new-head (v+ (first rope) dv)]
    (loop [new-rope [new-head]
           remaining-rope (rest rope)]
      (let [[head & tail] remaining-rope]
        (if (nil? head)
          new-rope
          (recur (conj new-rope (pull-tail (last new-rope) head))
               tail)))))) 

(defn simulate [instructions rope-length]
  (let [initial-state (repeat rope-length [0 0])]
    (reductions move-rope initial-state instructions)))

(defn count-tail-visits [steps]
  (->> steps
    (map last)
    (into #{})
    (count)))

(defn part-1 [input]
  (-> input
      parse-input
      (simulate 2)
      count-tail-visits))

(defn part-2 [input]
  (-> input
      parse-input
      (simulate 10)
      count-tail-visits))

(comment (pull-tail [12 12] [10 11]))

(comment (move-towards [2 1] [0 0]))

(comment (pprint (count-tail-visits (simulate (parse-input sample-input) 2))))

(comment (move-rope [[0 3] [0 2] [0 1]] [1 0]))

(comment (part-1 input))
(comment (part-2 input))