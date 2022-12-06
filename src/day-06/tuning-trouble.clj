(ns aoc22.day-06.tuning-trouble)

(defn first-distinct-seq [n coll]
  (->> (partition n 1 coll)
       (keep-indexed #(when (= n (count (set %2)))
                        %1))
       (first)
       (+ n)))

(def inp (slurp "src/day-06/inp.txt"))
(defn ex1 [text] (first-distinct-seq 4 text))
(defn ex2 [text] (first-distinct-seq 14 text))

(ex1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")    ;; 7
(ex2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")    ;; 19
(ex1 "bvwbjplbgvbhsrlpgdmjqwftvncz")      ;; 5
(ex2 "bvwbjplbgvbhsrlpgdmjqwftvncz")      ;; 23
(ex1 "nppdvjthqldpwncqszvftbrmjlhg")      ;; 6
(ex2 "nppdvjthqldpwncqszvftbrmjlhg")      ;; 23
(ex1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; 10
(ex2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") ;; 29
(ex1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  ;; 11
(ex2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")  ;; 26
(ex1 inp)                                 ;; 1538
(ex2 inp)                                 ;; 2315