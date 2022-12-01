(ns aoc22.day-01.calorie-counting (:require [clojure.string :as s]))

(def inp (slurp "src/day-01/inp.txt"))

(defn- elfs-calories [text]
  (->> (s/split-lines text)
       (reduce (fn [[curr-elf-cal elfs-cal] snack-cal]
                 (if (= "" snack-cal)
                   [0 (conj elfs-cal curr-elf-cal)]
                   [(+ (read-string snack-cal) curr-elf-cal) elfs-cal]))
               [0 []])
       (second)
       (sort >)))

(defn max-elf-cal [text] (->> (elfs-calories text) (first)))

(defn top-3-elf-cal [text] (->> (elfs-calories text) (take 3) (apply +)))

(max-elf-cal inp) ;; 69528
(top-3-elf-cal inp) ;; 206152
