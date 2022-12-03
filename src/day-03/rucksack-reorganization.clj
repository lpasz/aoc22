(ns aoc22.day-03.rucksack-reorganization
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def ex-inp (str "vJrwpWtwJgWrhcsFMMfFFhFp\n"
                 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n"
                 "PmmdzqPrVvPwwTWBwg\n"
                 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n"
                 "ttgJtRGJQctTZtZT\n"
                 "CrZsJsPPZsGzwwsLwLmpwMDw"))

(def inp (slurp "src/day-03/inp.txt"))

(def points (into {} (mapcat (fn [i] [[(char (+ i (int \a))) (+ i 1)]
                                      [(char (+ i (int \A))) (+ i 27)]])
                             (range 0 26))))

(defn ex1 [text]
  (->> (s/split-lines text)
       (reduce (fn [acc line]
                 (->> (-> (count line) (quot 2) (split-at (seq line)))
                      (map set)
                      (reduce set/intersection)
                      (first)
                      (points)
                      (+ acc))) 0)))

(defn ex2 [text]
  (->> (s/split-lines text)
       (partition 3)
       (reduce (fn [acc elves-group]
                 (let [[elf1 elf2 elf3] (map set elves-group)
                       inter (set/intersection elf1 elf2 elf3)]
                   (+ acc (points (first inter))))) 0)))


(ex1 ex-inp) ;; 157
(ex1 inp) ;; 7980
(ex2 ex-inp) ;; 70
(ex2 inp) ;; 2881