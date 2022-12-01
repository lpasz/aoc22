(ns aoc22.day-01.calorie-counting (:require [clojure.string :as s]))

(def inp (slurp "src/day-01/inp.txt"))

(defn- to-int [s] (Integer/parseInt s))

(defn- elf-total-calories [text]
  (->> (s/split text #"\n\n")
       (map #(->> (s/split-lines %) (map to-int) (apply +)))
       (sort >)))

(defn max-elf-cal [text] (->> (elf-total-calories text) (first)))

(defn top-3-elf-cal [text] (->> (elf-total-calories text) (take 3) (apply +)))

(max-elf-cal inp) ;; 69528
(top-3-elf-cal inp) ;; 206152