(ns aoc22.day-06.tuning-trouble)

(defn first-distinct-seq [n coll]
  (->> (partition n 1 coll)
       (keep-indexed #(when (apply distinct? %2) %1))
       (first)
       (+ n)))


(def ex-path "../inputs/day-06/ex.txt")
(def inp (slurp ex-path))
(defn ex1 [text] (first-distinct-seq 4 text))
(defn ex2 [text] (first-distinct-seq 14 text))

(ex1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")    ;; 7
(ex2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")    ;; 1
(ex1 "bvwbjplbgvbhsrlpgdmjqwftvncz")      ;; 5 
(ex2 "bvwbjplbgvbhsrlpgdmjqwftvncz")      ;; 23
(ex1 "nppdvjthqldpwncqszvftbrmjlhg")      ;; 6
(ex2 "nppdvjthqldpwncqszvftbrmjlhg")      ;; 23
(ex1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; 10
(ex2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; 29
(ex1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  ;; 11
(ex2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  ;; 26
(time (ex1 inp))                                 ;; 1538
(time (ex2 inp))                                 ;; 2315