(ns aoc22.day-02.rock-paper-scissors
  (:require [clojure.string :as s]))

(def rsp-encrypt {\A :rock \B :paper \C :scissor \X :rock \Y :paper \Z :scissor})

(def rules {:paper :rock :rock :scissor :scissor :paper})

(def points {:rock 1 :paper 2 :scissor 3 :win 6 :lose 0 :tie 3})

(def inp (slurp "src/day-02/inp.txt"))

(defn round-points [adv you]
  (cond ))

(defn get-points [[adv you]]
  (+ (points you) ()))

(defn parse-inp [text]
  (->> (s/split-lines text)
       (map (fn [line] (->> line (seq) ((juxt first #(nth % 2))))))
       (map get-points)))

(parse-inp inp)