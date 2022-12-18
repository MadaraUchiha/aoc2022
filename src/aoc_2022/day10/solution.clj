(ns aoc-2022.day10.solution 
  (:require
   [aoc-2022.helpers :as h]
   [clojure.string :as str]))

(def input (h/read-input 10))
(def sample (h/read-sample 10))

(comment
  '({:op :noop}
    {:op :addx :arg 15}))

(comment
  {:instructions '({:op :noop} {:op :addx :arg 15} ...)
   :x 1
   :cycle 1})

(defn parse-instruction [line]
  (let [[inst arg] (str/split line #" ")]
    (case inst
      "noop" {:op :noop}
      "addx" {:op :addx :arg (parse-long arg)})))
    

(defn parse-input [input]
  (map parse-instruction (str/split input #"\n")))

(defn initialize-computer [instructions initial-x]
  {:instructions instructions
   :x initial-x
   :cycle 0})

(defn delta-x [{:keys [op arg]}]
  ({:noop 0 :addx arg} op))

(def cycles
  {:noop 1
   :addx 2})

(defn execute-once [{:keys [instructions x cycle]}]
  (if (seq instructions)
    (let [[inst & rest-instructions] instructions
          {:keys [op]}               inst]
      {:instructions rest-instructions
       :x (+ x (delta-x inst))
       :cycle (+ cycle (cycles op))})
    nil))

(defn run [state]
  (take-while (comp not nil?) (iterate execute-once state)))

(defn cycle-state [stream cycle]
  (->> stream
       (take-while #(< (:cycle %) cycle))
       last
       :x))

(defn signal-strength [x cycle]
  (* x cycle))

(def interesting-cycles [20 60 100 140 180 220])

(defn part-1 [input]
  (let [stream (-> input
                   parse-input
                   (initialize-computer 1)
                   run)]
    (->> interesting-cycles
         (map #(cycle-state stream %))
         (map * interesting-cycles)
         (reduce +))))

(defn part-2 [input]
  (let [stream (-> input
                   parse-input
                   (initialize-computer 1)
                   run)]
    (->> (range 1 241)
         (map #(cycle-state stream %))
         (map-indexed (fn [i x] (h/within? (or x 1) (mod i 40) 1)))
         (map #(if % \# \.))
         (partition 40)
         (map (partial apply str))
         (str/join "\n"))))       
        

(comment
  (parse-input sample)
  (cycle-state (run (initialize-computer (parse-input sample) 1)) 60)
  (part-1 input)
  (println "")
  (println (part-2 input)))