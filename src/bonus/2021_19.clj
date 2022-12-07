(ns bonus.2021-19 
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :refer [split]]
   [com.rpl.specter :refer [ALL MAP-VALS transform]]))

(def input (slurp "src/bonus/2021_19.txt"))

;; TODO: Decide on a data structure and write it down
;; TODO: Once that's done, parse input to get to that data structure

(defn parse-scanner-block [scanner-block]
	(let [[title & beacons] (split scanner-block #"\n")
		  scanner-id (second (re-find #"^--- scanner (\d+) ---$" title))]
		(->> {(parse-long scanner-id) beacons}
			 (transform [MAP-VALS ALL] #(split % #","))
			 (transform [MAP-VALS ALL ALL] parse-long))))
		 ; (->> beacons
		 ; 			   (map #(split % #","))
		 ; 			   (transform [ALL ALL] parse-long))}))

(defn parse-input [input]
	(->> (split input #"\n\n")
		 (map parse-scanner-block)
		 (into {})))

(defn all-pairs [sq] 
	(->> (for [i sq j sq] [i j])
		 (filter #(not= (first %) (second %)))))

(defn square [n] (* n n))

(defn square-distance [[x1 y1 z1] [x2 y2 z2]]
	(+ (square (- x2 x1))
	   (square (- y2 y1))
	   (square (- z2 z1))))

(defn find-neighbor-distances [points]
	(->> points
		all-pairs
		(map (fn [[from to]] [from [to (square-distance from to)]]))
		(group-by first)
		(transform [MAP-VALS ALL] second)
		(transform [MAP-VALS] #(into {} %))))

(defn find-first-overlap [scanner-map]
	(->> (range (count scanner-map))
		 all-pairs
		 (transform [ALL ALL] #(get scanner-map %))
		 (take 3)))

(comment (macroexpand '(->> foo (bar) (baz)))
	(parse-input input)
	(all-pairs [[1 1 1] [2 2 2] [3 3 3] [4 4 4]])
	(pprint (find-neighbor-distances [[1 1 1] [2 2 2] [3 3 3] [4 4 4]]))
	(pprint (find-first-overlap (parse-input input))))