(ns aoc22.day-11.monkey-in-the-middle
  (:require [clojure.string :as s]))

(def inp (slurp "src/day-11/inp.txt"))
(def ex-inp (slurp "src/day-11/ex-inp.txt"))

(defn to-int [i] (Integer/parseInt i))

(defn parse-monkey-number [monkey-number]
  (Integer/parseInt (s/replace monkey-number #"[^0-9]" "")))

(defn parse-starting-items [starting-items]
  (->> (s/split (last (s/split starting-items #": ")) #", ")
       (map to-int)
       (into [])
       (new java.util.ArrayList)))

(defn parse-operation [operation]
  (-> (str "(fn [old] ("
           (s/replace operation #"Operation: new = old " "")
           " old ))")
      (read-string)
      (eval)))

(defn parse-test [test]
  (let [tests (-> (apply str test) (s/replace #"[^0-9]+" ",") (s/split #","))
        [divisible-for m1 m2] (->> tests (drop 1) (map to-int))]
    {:test #(if (zero? (rem % divisible-for)) m1 m2)
     :divisible-for divisible-for}))

(defn parse-line [line]
  (let [[monkey-number starting-items operation & test] (s/split-lines line)]
    (merge {:items (parse-starting-items starting-items)
            :operation (parse-operation operation)
            :inspected (volatile! 0)}
           (parse-test test))))

(defn parse-inp [text]
  (->> (s/split text #"\n\n")
       (map parse-line)
       (into [])))

(defn inspect-worry-throw [{:keys [items operation test inspected]} monkeys relief denominator]
  (do (vswap! inspected + (count items))
      (doseq [item items]
        (let [worry-lvl (operation item)
              worry-lvl (quot worry-lvl relief)
              worry-lvl (rem worry-lvl denominator)
              next-monkey-items (:items (monkeys (test worry-lvl)))]
          (. next-monkey-items add worry-lvl)))
      (. items clear)))

(defn do-turns [text turns relief]
  (let [monkeys (parse-inp text)
        denominator (->> monkeys (map :divisible-for) (reduce *))]
    (dotimes [_ turns]
      (doseq [monkey monkeys]
        (inspect-worry-throw monkey monkeys relief denominator)))
    (->> monkeys
         (map #(deref (:inspected %)))
         (sort >)
         (take 2)
         (apply *))))

(defn ex1 [text] (do-turns text 20 3))
(defn ex2 [text] (do-turns text 10000 1))

(ex1 ex-inp) ;; 10605
(ex2 ex-inp) ;; 2713310158
(ex1 inp)    ;; 55458
(ex2 inp)    ;; 14508081294

(inc (volatile! 0))
