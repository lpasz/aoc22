(ns aoc22.day-05.supply-stacks
  (:require [clojure.string :as s]))

(def ex-inp (slurp "src/day-05/ex-inp.txt"))
(def inp (slurp "src/day-05/inp.txt"))

(defn transpose [matrix] (apply map vector matrix))

(defn parse-stacks [stacks]
  (->> (s/split stacks #"\n")
       (map seq)
       (transpose)
       (map reverse)
       (filter #(not= (first %) \space))
       (reduce (fn [acc stack]
                 (assoc acc
                        (first stack)
                        (filter #(not= % \space) (rest stack)))) {})))

(defn parse-moves [moves]
  (->> (s/split moves #"[^0-9]")
       (filter not-empty)
       (partition 3)))

(defn parse-input [[stacks moves]]
  [(parse-stacks stacks)
   (parse-moves moves)])

(defn do-calc [[stacks moves]]
  (reduce (fn [stacks [quant from to]]
            (-> stacks
                (assoc from (drop quant (stacks from)))
                (assoc to  (concat (take quant (stacks from)) (stacks to))))) stacks moves))

(defn ex1 [text]
  (->> (s/split text #"\n\n")
       (parse-input)
       (do-calc)))

(ex1 ex-inp)