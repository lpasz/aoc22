(ns aoc22.day-21.monkey-math
  (:require [clojure.string :as s]))

(def ex-inp (slurp "lib/day-21/ex-inp.txt"))
(def inp (slurp "lib/day-21/inp.txt"))

(defn parse [text]
  (->> (s/split-lines text)
       (map (fn [line]
              (s/split line #": | ")))
       (reduce (fn [acc [name & rest]]
                 (if (= (count rest) 1)
                   (assoc acc (keyword name)
                          (Integer/parseInt (first rest)))
                   (assoc acc (keyword name)
                          [(keyword (first rest))
                           (read-string (second rest))
                           (keyword (last rest))])))
               {})))

(def ex-monkey-math-map (parse ex-inp))
(def monkey-math-map (parse inp))

(defn find-recur-monkey-math [value acc]
  (if (number? value)
    value
    (let [[val1 operation val2] value]
      (list operation
            (find-recur-monkey-math (val1 acc) acc)
            (find-recur-monkey-math (val2 acc) acc)))))

(eval (find-recur-monkey-math (:root monkey-math-map) monkey-math-map))
