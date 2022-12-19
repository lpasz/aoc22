(ns aoc22.day-19.not-enough-minerals
  (:require [clojure.string :as s]))

(defonce ex-inp (slurp "lib/day-19/ex-inp.txt"))
(defonce inp (slurp "lib/day-19/inp.txt"))

(defn parse [text]
  (->>   (s/split text #"[^0-9]")
         (keep not-empty)
         (map #(Integer/parseInt %))
         (partition 7)
         (map (fn [[n oroc croc obroc obrcc groc grobc]]
                {:number n
                 :ore-robot {:ore oroc}
                 :clay-robot {:ore croc}
                 :obisidian-robot {:ore obroc :clay obrcc}
                 :geode-robot {:ore groc :obisidian grobc}}))))

(defonce ex-blueprints (parse ex-inp))
(defonce blueprints (parse inp))

(def robots-collect {:ore-robot :ore
                     :clay-robot :clay
                     :obsidian-robot :obisidian
                     :geode-robot :geode})

(def start-robots {:ore-robot 1
                   :clay-robot 0
                   :obsidian-robot 0
                   :geode-robot 0})

(def start-materials {:ore 0
                      :clay 0
                      :obisidian 0
                      :geode 0})

(defn round-robot-collect [curr-robots materials]
  (reduce (fn [acc [robot count]]
            (update acc (robot robots-collect) #(+ %1 count)))
          materials curr-robots))

(defn robot-builder [robot-type]
  (fn [time robots materials blueprint]
    (let [req-material-cost (robot-type blueprint)
          req-material-types (keys req-material-cost)]
      (if (every? #(>= (%1 materials) (%1 req-material-cost)) req-material-types)
        [(inc time)
         (update robots robot-type inc)
         (reduce (fn [acc itm]
                   (update acc itm #(- %1 (itm req-material-cost))))
                 materials
                 req-material-types)]))))

((robot-builder :ore-robot)
 1
 start-robots
 {:ore 2 :clay 0 :obisidian 0 :geode 0}
 (first ex-blueprints))

(defn next-possible-actions [time robots materials blueprint]
  (->>  [[(inc time) robots materials]
         ((robot-builder :ore-robot) time robots materials blueprint)
         ((robot-builder :clay-robot) time robots materials blueprint)
         ((robot-builder :obsidian-robot) time robots materials blueprint)
         ((robot-builder :geode-robot) time robots materials blueprint)]
        (keep not-empty)
        (into #{})))

(defn best-order [blueprint]
  (loop [info [[0 start-robots start-materials]]
         max-geode 0]
    (let [[[time robots materials] & tl] info]
      (cond (empty? info) max-geode

            (>= time 24) (recur tl (max max-geode (:geode materials)))

            :else (let [nmaterials (round-robot-collect robots materials)
                        next-infos (next-possible-actions time robots nmaterials blueprint)]
                    (recur (concat tl next-infos) max-geode))))))


(best-order (first ex-blueprints))

(:geode-robot (first ex-blueprints))
;; 7 turn with 1 ob + 2 turn 1 ore




