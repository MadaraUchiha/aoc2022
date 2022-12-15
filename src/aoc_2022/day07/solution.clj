(ns aoc-2022.day07.solution 
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]))

(def input (slurp "src/aoc_2022/day07/input.txt"))

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")

(def initial-fs {:pwd [] :dirs {}})
(def total-fs-size 70000000)
(def desired-fs-size 30000000)

(defn map-values [f coll]
  (->> coll
    (map (fn [[k v]] [k (f v)]))
    (into {})))

(defn create-file 
  "Creates the file in the path, and all parent paths"
  [fs path file]
  (->> (range) ;; 0 1 2 3 4 5 6 ...
    (take (count path)) ;; 0 1 2
    (map inc) ;; 1 2 3
    (map #(subvec path 0 %)) ;; ["a"] ["a" "b"] ["a" "b" "c"]
    (reduce (fn [acc curr] (update-in acc [:dirs curr] conj file)) fs)))

(defn apply-command [{:keys [pwd] :as fs} command]
  (let [[cmd1 cmd2 cmd3] (str/split command #" ")]
    (cond
      (parse-long cmd1)           (create-file fs pwd {:name cmd2 :size (parse-long cmd1)})
      (= [cmd1 cmd2] ["$" "cd"])  (update fs :pwd (if (= cmd3 "..") pop #(conj % cmd3)))
      :else                       fs))) ;; other commands ignored

(defn parse-input [input]
  (->> (str/split input #"\n")
       (reduce apply-command initial-fs)
       :dirs))

(defn part-1 
  "Find the sum of all folders (incl. subfolders) with size smaller than 100000"
  [input]
  (->> input
    parse-input
    (map-values #(map :size %))
    (map-values #(reduce + %))
    (map second)
    (filter #(> 100000 %))
    (reduce +)))

(defn part-2 
  "Find smallest directory to delete to free up enough space"
  [input]
  (let [dir-to-size    (->> input
                         parse-input
                         (map-values #(map :size %))
                         (map-values #(reduce + %)))
        total-occupied (get dir-to-size ["/"])
        total-free     (- total-fs-size total-occupied)
        to-free        (- desired-fs-size total-free)]
        
    (->> dir-to-size
      (map second)
      (filter #(< to-free %))
      (sort)
      (first))))

(comment
  (-> initial-fs
      (apply-command "$ cd /")
      (apply-command "dir a")
      (apply-command "$ cd a")
      (apply-command "dir b")
      (apply-command "123 c.txt")
      (apply-command "$ cd .."))
  (pp/pprint (parse-input sample-input)))

(comment (pp/pprint (part-1 input)))
(comment (pp/pprint (part-2 input)))