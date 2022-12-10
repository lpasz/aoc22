(ns aoc22.day-10.cathode-ray-tube
  (:require [clojure.string :as s]))

(def inp (slurp "src/day-10/inp.txt"))
(def ex-inp-01 (slurp "src/day-10/ex-inp-01.txt"))
(def ex-inp-02 (slurp "src/day-10/ex-inp-02.txt"))

(defn to-instruction [line]
  (if (= "noop" line)
    [:noop]
    [:noop
     (->> (s/split line #" ")
          (last)
          (Integer/parseInt))]))

(to-instruction "addx 3")

(defn parse-inp [text]
  (->> (s/split-lines text)
       (mapcat to-instruction)
       (reductions (fn [[cycle reg] instruction]
                     (if (= :noop instruction)
                       [(inc cycle) reg]
                       [(inc cycle) (+ instruction reg)]))
                   [1 1])
       (into (sorted-map))))

(def power-cycles '(20 60 100 140 180 220))

(defn ex1 [text]
  (let [m (parse-inp text)]
    (loop [pcs power-cycles
           sum 0]
      (let [pc (first pcs)]
        (if-let [t (m pc)]
          (recur (rest pcs)
                 (+ sum (* pc t)))
          sum)))))

(defn bit-on? [cycle reg]
  (#{reg (+ 1 reg) (+ 2 reg)} (rem cycle 40)))

(defn ex2 [text]
  (let [m (parse-inp text)]
    (->> m
         (map (fn [[cycle reg]]
                (if (bit-on? cycle reg) \# \.)))
         (partition 40)
         (map #(apply str %)))))

(ex1 ex-inp-02)
(ex1 inp)
(ex2 ex-inp-02)
(ex2 inp)
