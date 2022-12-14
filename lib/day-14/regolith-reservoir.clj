(ns aoc22.day-14.regolith-reservoir
  (:require [clojure.string :as s]))

(def inp (slurp "lib/day-14/inp.txt"))
(def ex-inp (slurp "lib/day-14/ex-inp.txt"))

(defn rrange [r1 r2]
  (if (< r1 r2)
    (range r1 (inc r2))
    (range r2 (inc r1))))

(defn rock-edge [[from-x from-y] [to-x to-y]]
  (let [x-range (rrange from-x to-x)
        y-range (rrange from-y to-y)]
    (if (< (count x-range) (count y-range))
      (partition 2 (interleave (repeat (first x-range)) y-range))
      (partition 2 (interleave  x-range (repeat (first y-range)))))))

(rock-edge [498 6] [496 6])

(defn add-rock-edge [acc [from to]]
  (->> (rock-edge from to)
       (reduce #(assoc %1 (vec %2) :rock) acc)))

(defn to-int [i] (Integer/parseInt i))

(defn parse-line [acc line]
  (->> (s/split line #" -> |,")
       (map to-int)
       (partition 2)
       (partition 2 1)
       (reduce add-rock-edge acc)))

(parse-line {} "498,4 -> 498,6 -> 496,6")

(defn parse-inp [text]
  (->> (s/split-lines text)
       (reduce parse-line {})))

(defn down [[sx sy]] [sx (inc sy)])
(defn left-diagonal [[sx sy]] [(dec sx) (inc sy)])
(defn right-diagonal [[sx sy]] [(inc sx) (inc sy)])


(defn move-sand [acc sand-pos]
  (cond
    (nil? (acc (down sand-pos))) (down sand-pos)
    (nil? (acc (left-diagonal sand-pos))) (left-diagonal sand-pos)
    (nil? (acc (right-diagonal sand-pos))) (right-diagonal sand-pos)))

(defn abyss-sand-fall-sim [acc sand-start free-fall-after]
  (loop [blockers acc
         sand-pos sand-start]
    (let [next-sand-pos (move-sand blockers sand-pos)]
      (cond
        (nil? next-sand-pos) (assoc blockers sand-pos :sand)
        (> (second next-sand-pos) free-fall-after) :stop
        :else (recur blockers next-sand-pos)))))

(defn floor-sand-fall-sim [acc sand-start infinity-floor]
  (loop [blockers acc
         sand-pos sand-start]
    (let [next-sand-pos (move-sand blockers sand-pos)]
      (cond
        (= :sand (acc sand-start)) :stop
        (nil? next-sand-pos) (assoc blockers sand-pos :sand)
        (= (second next-sand-pos) infinity-floor) (assoc blockers sand-pos :sand)
        :else (recur blockers next-sand-pos)))))

(defn sand-max-capacity [sim quirk acc]
  (loop [acc acc]
    (let [next-acc (sim acc [500 0] quirk)]
      (if (= next-acc :stop)
        (count (filter #(= %1 :sand) (vals acc)))
        (recur next-acc)))))

(defn ex1 [text]
  (let [rocks (parse-inp text)
        free-fall-after (apply max (map second (keys rocks)))]
    (sand-max-capacity abyss-sand-fall-sim free-fall-after rocks)))


(defn ex2 [text]
  (let [rocks (parse-inp text)
        infinity-floor-y (+ 2 (apply max (map second (keys rocks))))]
    (sand-max-capacity floor-sand-fall-sim infinity-floor-y rocks)))

(ex1 ex-inp) ;; 24
(ex1 inp) ;; 1003

(ex2 ex-inp) ;; 93
(ex2 inp) ;; 25771

