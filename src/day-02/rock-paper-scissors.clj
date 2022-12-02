(ns aoc22.day-02.rock-paper-scissors (:require [clojure.string :as s]))

(def ex1-encrypt {\A :rock \B :paper \C :scissor \X :rock \Y :paper \Z :scissor})
(def ex2-encrypt {\X :lose \Y :tie \Z :win})
(def win {:paper :rock :rock :scissor :scissor :paper})
(def win-of (clojure.set/map-invert win))
(def points {:rock 1 :paper 2 :scissor 3 :win 6 :lose 0 :tie 3})
(def inp (slurp "src/day-02/inp.txt"))

(defn game-result [adv you]
  (cond (= adv you) :tie
        (= adv (win you))  :win
        (= (win adv) you) :lose))

(defn get-points [[adv you]] (+ (points you) (points (game-result adv you))))

(defn my-play [adv result]
  (cond (= :tie result) adv
        (= :win result) (win-of adv)
        (= :lose result) (win adv)))

(defn make-result-happen [[adv result]] (+ (points result) (points (my-play adv result))))

(defn ex1 [text]
  (->> (s/split-lines text)
       (map (fn [line] (->> (seq line)
                            ((juxt #(ex1-encrypt (first %))
                                   #(ex1-encrypt (nth % 2)))))))
       (map get-points)
       (apply +)))

(defn ex2 [text]
  (->> (s/split-lines text)
       (map (fn [line] (->> (seq line)
                            ((juxt #(ex1-encrypt (first %))
                                   #(ex2-encrypt (nth % 2)))))))
       (map make-result-happen)
       (apply +)))

(ex1 inp) ;; 14531
(ex2 inp) ;; 11258