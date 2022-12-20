(ns aoc-2022.day11.solution 
  (:require
    [aoc-2022.helpers :as h]
    [clojure.string :as str]
    [clojure.pprint :as pp]))

(def input (h/read-input 11))
(def sample (h/read-sample 11))

(comment
  {0 {:id 0 :items [79 98] :operation #(* % 19) :test 23 :true? 2 :false? 3}
   1 {... ...}})

(defn formula-to-fn [formula]
  (let [[arg1 op arg2] (str/split formula #" ")
        op             (if (= op "*") * +)]
    (fn [old]
      (op
        (if (= arg1 "old") old (parse-long arg1))
        (if (= arg2 "old") old (parse-long arg2))))))

(defn parse-monkey [monkey-str]
  (let [[id items op test true? false?] (str/split monkey-str #"\n")
        id-str     (h/re-extract #"^Monkey (\d+):$" id)
        items-str  (last (str/split items #": "))
        op-str     (last (str/split op #" = "))
        test-str   (last (str/split test #" "))
        true?-str  (last (str/split true? #" "))
        false?-str (last (str/split false? #" "))]
    {:id        (parse-long id-str)
     :items     (mapv parse-long (str/split items-str #", "))
     :op        (formula-to-fn op-str)
     :test      (parse-long test-str)
     :true?     (parse-long true?-str)
     :false?    (parse-long false?-str)
     :inspected 0}))
        
(defn parse-input [input]
  (->> (str/split input #"\n\n")
       (map parse-monkey)
       (map (fn [m] [(:id m) m]))
       (into {})))

(defn throw-item [monkeys from to]
  (let [[item & rest] (:items (monkeys from))]
    (-> monkeys
        (assoc-in [from :items] (into [] rest))
        (update-in [to :items] conj item))))

(defn inspect [monkeys id]
  (let [monkey                               (monkeys id)
        {:keys [items op test true? false?]} monkey
        [item]                               items
        new-worry (-> item op)]
    (-> monkeys
        (assoc-in [id :items 0] new-worry)
        (update-in [id :inspected] inc)
        (throw-item id (if (zero? (mod new-worry test)) true? false?)))))

(defn inspect-all [monkeys id]
  (loop [remaining-items (-> (monkeys id) :items count)
         new-monkeys monkeys]
    (if (zero? remaining-items)
      new-monkeys
      (recur
        (dec remaining-items)
        (inspect new-monkeys id)))))
       
(defn play-round [monkeys]
  (loop [i 0
         new-monkeys monkeys]
    (if (= i (count monkeys))
      new-monkeys
      (recur
        (inc i)
        (inspect-all new-monkeys i)))))

(defn monkey-business [monkeys]
  (->> monkeys
       (map second)
       (map :inspected)
       sort
       reverse
       (take 2)
       (reduce *)))

(defn div3 [n] (quot n 3))

(defn part-1 [input]
  (as-> input $
      (parse-input $)
      (h/map-values #(update % :op (partial comp div3)) $)
      (iterate play-round $)
      (nth $ 20)
      (monkey-business $)))

(defn part-2 [input]
  (let [monkeys (parse-input input)
        max-worry (apply * (map (comp :test second) monkeys))
        mod-max #(mod % max-worry)]
    (as-> monkeys $
          (h/map-values #(update % :op (partial comp mod-max)) $)
          (iterate play-round $)
          (nth $ 10000)
          (monkey-business $))))

(comment
  (-> input
      part-1
      pp/pprint))

(comment
  (-> input
      part-2
      pp/pprint))
