(ns aoc-2022.day13.solution 
  (:require
   [clojure.edn :as edn]
   [clojure.string :as s]
   [aoc-2022.helpers :as h]
   [clojure.pprint :as pp]))

(def input (h/read-input 13))
(def sample (h/read-sample 13))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
    (mapv s/split-lines)
    (mapv #(mapv edn/read-string %))))

(defn parse-input-2 [input]
  (->> (s/split-lines input)
       (filter (comp not empty?))
       (mapv edn/read-string)))

(defmulti compare-part (fn [left right] 
                         [(vector? left) (vector? right)]))
(defmethod compare-part [false false] both-numbers [left right]
  (- right left))
(defmethod compare-part [true false] first-vec [left right]
  (compare-part left [right]))
(defmethod compare-part [false true] second-vec [left right]
  (compare-part [left] right))
(defmethod compare-part [true true] both-vecs [left right]
  (let [result (cond (zero? (count left)) 1
                     (zero? (count right)) -1
                     :else  (->> (map compare-part left right)
                                 (filter (comp not zero?))
                                 first))]
    (or result (- (count right) (count left)))))



(defn part-1 [input]
  (->> input
    parse-input
    (map-indexed (fn [idx [left right]] [(inc idx) (compare-part left right) [left right]]))
    (filter (comp pos? second))
    (map first)
    (apply +)))

(defn part-2 [input]
  (->> input
    parse-input-2
    (#(concat % [[[2]] [[6]]]))
    (sort #(compare-part %1 %2))
    reverse
    (map-indexed (fn [i x] [(inc i) x]))
    (filter #(or (= (second %) [[2]]) (= (second %) [[6]])))
    (map first)
    (apply *)))
    
    

(comment
  (parse-input sample)
  (into [] [2])
  (compare-part [1] 2)
  (compare-part [1 1 3 1 1] [1 1 5 1 1])
  (pp/pprint (part-1 input))
  (pp/pprint (part-2 input)))