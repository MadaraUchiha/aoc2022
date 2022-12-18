(ns aoc-2022.day08.solution 
  (:require
   [aoc-2022.helpers :refer [filter-keys map-values]]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))

(def input (slurp "src/aoc_2022/day08/input.txt"))

(def sample-input) 
"30373
 25512
 65332
 33549
 35390"

(comment
  {[0 0] {:height 1}
   [1 0] {:height 2}})

(defn parse-input [input]
  (let [lines (str/split input #"\n")
        height (count lines)
        width (count (first lines))
        grid (into {} (for [x (range width) y (range height)]
                        [[x y] {:height (-> lines
                                            (nth y)
                                            (nth x)
                                            (str)
                                            (parse-long))}]))]
    {:height height :width width :grid grid}))

(defn all-smaller? [n coll]
  (every? #(> n %) coll))

(defn visible? [{:keys [grid width height]} [x y]]
  (let [tree-height (:height (grid [x y]))
        h-grid (map-values :height grid)]
    (or (all-smaller? tree-height (->> (range (dec x) -1 -1) (map #(h-grid [% y]))))
        (all-smaller? tree-height (->> (range (inc x) width) (map #(h-grid [% y]))))
        (all-smaller? tree-height (->> (range (dec y) -1 -1) (map #(h-grid [x %]))))
        (all-smaller? tree-height (->> (range (inc y) height) (map #(h-grid [x %])))))))
    
(defn take-while-incl-last
  [pred coll]
  (lazy-seq
    (when-let [[f & r] (seq coll)]
      (if (pred f)
        (cons f (take-while-incl-last pred r))
        [f]))))

(defn scenic-score [{:keys [grid width height]} [x y]]
    (let [tree-height (:height (grid [x y]))
          h-grid      (map-values :height grid)
          get-row     (fn [from to step] (map #(h-grid [% y]) (range from to step)))
          get-col     (fn [from to step] (map #(h-grid [x %]) (range from to step)))]
      (reduce * [(->> (get-row (dec x) -1 -1) (take-while-incl-last #(> tree-height %)) count)
                 (->> (get-row (inc x) width 1) (take-while-incl-last #(> tree-height %)) count)
                 (->> (get-col (dec y) -1 -1) (take-while-incl-last #(> tree-height %)) count)
                 (->> (get-col (inc y) height 1) (take-while-incl-last #(> tree-height %)) count)])))

(defn part-1 [input]
  (let [forest (parse-input input)]
    (->> forest
         :grid
         (filter-keys #(visible? forest %))
         ; (sort-by first)
         count)))

(defn part-2 [input]
  (let [forest (parse-input input)]
    (->> forest
         :grid
         (map (fn [[k]] (scenic-score forest k)))
         (apply max))))
  

(comment (-> input
             part-1
             pprint))
             
(comment (-> input part-2))

(comment (visible? (parse-input input) [1 1]))

