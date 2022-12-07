(ns aoc-2022.day07.solution 
  (:require
   [clojure.string :as str]
   [com.rpl.specter :refer [recursive-path if-path continue-then-stay MAP-VALS ALL FIRST LAST]]))

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

(comment "sample structure"
  { :pwd  ["/" "a" "e"]
    :dirs {"/" {"a" {"e" {"i" 584}
                    "f" 29116
                    "g" 2557
                    "h.lst" 62596
               "b.txt" 14848514}}}})

(comment "alternative structure"
    {:pwd ["/" "a" "e"]
     :dirs [{:path "//a/e/i" :size 584}
            {:path "//a/f"   :size 29116}]})

(defn apply-command [{:keys [pwd] :as fs} command]
  (let [[cmd1 cmd2 cmd3] (str/split command #" ")]
    (cond
      ; (= cmd1 "dir")              (assoc fs (str/join "/" pwd) {})
      (parse-long cmd1)           (assoc-in fs [:dirs (str/join "/" (conj pwd cmd2))] (parse-long cmd1))
      (= [cmd1 cmd2] ["$" "cd"])  (update fs :pwd (if (= cmd3 "..") pop #(conj % cmd3)))
      :else-other-commands        fs)))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (reduce apply-command initial-fs)))

(def MAP-NODES
   (recursive-path [] p
     (if-path map?
     (continue-then-stay MAP-VALS p))))

(def map-key-walker (recursive-path [akey] p [ALL (if-path [FIRST #(= % akey)] LAST [LAST p])]))



(comment
  (-> initial-fs
      (apply-command "$ cd /")
      (apply-command "dir a")
      (apply-command "$ cd a")
      (apply-command "dir b")
      (apply-command "123 c.txt")
      (apply-command "$ cd .."))
  (pp/pprint (parse-input sample-input))
  )