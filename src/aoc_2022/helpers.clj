(ns aoc-2022.helpers)

(defn map-keys [f coll]
  (->> coll
    (map (fn [[k v]] [(f k) v]))
    (into {})))

(defn map-values [f coll]
  (->> coll
    (map (fn [[k v]] [k (f v)]))
    (into {})))  


(defn filter-keys [f coll]
  (->> coll
    (filter (fn [[k]] (f k)))  
    (into {})))

(defn filter-values [f coll]
  (->> coll
    (filter (fn [[_ v]] (f v)))  
    (into {})))

(defn minv [coll]
  (apply min (conj coll ##Inf)))

(defn maxv [coll]
  (apply max (conj coll ##-Inf)))

(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn pairwise [coll]
  (partition 2 1 coll))

(defn read-input [day]
  (slurp (format "src/aoc_2022/day%02d/input.txt" day)))

(defn read-sample [day]
  (slurp (format "src/aoc_2022/day%02d/sample.txt" day)))

(defn within? [n target epsilon]
  (-> target
      (- n)
      abs
      (<= epsilon)))

(defn re-extract [re s]
  (second (re-find re s)))