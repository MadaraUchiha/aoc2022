(defproject aoc-2022 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2022 - In Clojure"
  :url "https://github.com/MadaraUchiha/aoc2022"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :main ^:skip-aot aoc-2022.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})