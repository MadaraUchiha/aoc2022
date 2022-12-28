(ns aoc-2022.day12.solution 
  (:require
   [aoc-2022.helpers :as h :refer [filter-values v-]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))

(def input (h/read-input 12))
(def sample (h/read-sample 12))

(def infinity 99999)

(comment 
  {[0 0] 0
   [0 1] 1})

(defn parse-input [input]
  (let [grid  (->> input
                   str/split-lines
                   (map-indexed (fn [y line]
                                  (map-indexed (fn [x c] [[x y] (int c)]) line)))
                   (apply concat)
                   (into (sorted-map)))
        start (->> grid
                   (h/filter-values #(= (int \S) %))
                   first first)
        end   (->> grid
                   (h/filter-values #(= (int \E) %))
                   first first)]
    {:start start 
     :end end 
     :grid (-> grid
               (assoc start (int \a) end (int \z)))}))

(defn walkable? [from to]
  (< (- (or to infinity) from) 2))

(defn walkable-neighbors [grid v]
  (let [h (grid v)]
    (->> [[0 1] [1 0] [0 -1] [-1 0]]
      (map #(h/v+ v %))
      (filter #(walkable? h (grid %))))))

(defn estimate-distance [v1 v2]
  (->> (v- v1 v2)
    (map abs)
    (apply +)))

(defn a* [{:keys [start end grid]}]
  (loop [frontier    (priority-map start 0)
         cost-so-far {start 0}]
    (if (empty? frontier)
      infinity
      (let [[current]       (peek frontier)
            rest-frontier   (pop frontier)]
        (if (= current end)
          (cost-so-far current) ;; end
          (let [walkables             (walkable-neighbors grid current)
                walkable-costs        (map (fn [next] [next (inc (cost-so-far current))]) walkables)
                interesting-walkables (filter (fn [[next cost]] (< cost (or (cost-so-far next) infinity))) walkable-costs)
                estimated-costs       (map (fn [[next cost]] [next (+ cost (estimate-distance next end))]) interesting-walkables)]
            (recur (into rest-frontier estimated-costs)
                   (into cost-so-far interesting-walkables))))))))

(defn part-1 [input]
  (-> input
    parse-input
    a*))

(defn part-2 [input]
  (let [{:keys [grid end] :as game} (parse-input input)
        lowest-tiles (->> grid
                          (filter-values #(= % (int \a)))
                          (map first)
                          (sort-by #(estimate-distance % end))
                          reverse)]
    ; (pprint lowest-tiles)))
    (loop [min-distance infinity
           [current & rest] lowest-tiles]
      (pprint (str "Trying " current " cutoff: " min-distance))
      (if (nil? current) 
        min-distance
        (recur (min min-distance (a* (assoc game :start current)))
               rest)))))
      

(comment
  (let [{:keys [start end grid]} (parse-input sample)]
    (walkable-neighbors grid [2 0]))
  (assoc (priority-map :a 2 :b 1) :b 3)
  (estimate-distance [1 1] [5 -5])
  (let [[first & rest] (priority-map :a 3 :b 2 :c 1)]
    [first rest])
  (part-1 input)
  (part-2 input))
    
  