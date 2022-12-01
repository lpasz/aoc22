(ns aoc22.day-01.calorie-counting
  (:require [clojure.string :as s]))

(def inp (slurp "src/day-01/inp.txt"))

(defn max-elf-cal [text]
  (->> (s/split-lines text)
       (reduce (fn [[curr-elf-cal max-elf-cal] snack-cal]
                 (if (= "" snack-cal)
                   [0 (max max-elf-cal curr-elf-cal)]
                   [(+ (read-string snack-cal) curr-elf-cal) max-elf-cal]))
               [0 0])
       (second)))

(defn top-3-max-elf-cal [text]
  (->> (s/split-lines text)
       (reduce (fn [[curr-elf-cal elfs-cal] snack-cal]
                 (if (= "" snack-cal)
                   [0 (conj elfs-cal curr-elf-cal)]
                   [(+ (read-string snack-cal) curr-elf-cal) elfs-cal]))
               [0 []])
       (second)
       (sort >)
       (take 3)
       (apply +)))

(max-elf-cal inp) ;;69528
(top-3-max-elf-cal inp) ;; 206152

(time (top-3-max-elf-cal inp))
