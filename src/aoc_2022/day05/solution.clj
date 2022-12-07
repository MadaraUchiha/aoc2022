(ns aoc-2022.day05.solution 
  (:require
   [clojure.string :refer [split]]))

(def sample-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(def input (slurp "src/aoc_2022/day05/input.txt"))

(defn stack-offset [i]
  (-> i (* 4) (+ 1)))

(defn find-offset [n]
  (-> n (- 1) (stack-offset)))

(defn get-container-stack [lines offset]
  (->> lines
       (map #(nth % offset))
       (filter #(not= \space %))
       (reverse)))

(defn parse-abomination [abomination]
  (let [[names & containers] (reverse (split abomination #"\n"))
        stacks               (-> names (count) (+ 1) (/ 4))
        offsets              (map stack-offset (range stacks))
        names                (map (comp parse-long str (partial nth names)) offsets)]
    (zipmap names (map #(get-container-stack containers (find-offset %)) names))))

(defn parse-step [str]
  (let [pattern #"^move (\d+) from (\d+) to (\d+)$"]
    (zipmap [:n :from :to]
            (->> str
                 (re-find pattern)
                 (rest)
                 (map parse-long)))))

(defn parse-input [input]
  (let [[abomination moves] (split input #"\n\n")]
    {:cargo (parse-abomination abomination)
     :steps (map parse-step (split moves #"\n"))}))

(defn step [cargo {:keys [from to n]} order-fn]
  (let [moved-containers (order-fn (take n (get cargo from)))]
    (-> cargo
      (update from #(drop n %))
      (update to #(apply conj % moved-containers)))))

(defn step-one-at-a-time 
  "Move containers with one step at a time strategy"
  [cargo moves]
  (step cargo moves identity))

(defn step-multiple 
  "Move containers with move many at once strategy"
  [cargo moves]
  (step cargo moves reverse))

(defn render-message [cargo]
  (->> (range (count cargo))
       (map inc)
       (map #(first (get cargo %)))
       (apply str)))

(defn part-1 [input]
  (let [{:keys [cargo steps]} (parse-input input)
        final-cargo           (reduce step-one-at-a-time cargo steps)]
    (render-message final-cargo)))

(defn part-2 [input]
  (let [{:keys [cargo steps]} (parse-input input)
        final-cargo           (reduce step-multiple cargo steps)]
    (render-message final-cargo)))

(comment
  (parse-abomination (first (split input #"\n\n")))
  (stack-offset 8)
  (parse-step "move 1 from 3 to 5")
  (def parsed-input (parse-input input))
  (part-1 input)
  (part-2 input)
  )