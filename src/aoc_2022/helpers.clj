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