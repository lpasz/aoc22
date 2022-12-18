(ns aoc22.day-18.boiling-boulders
  (:require [clojure.string :as s]
            [clojure.core.reducers :as r]
            [clojure.pprint :as pp]))

(def ex-inp (slurp "lib/day-18/ex-inp.txt"))
(def inp (slurp "lib/day-18/inp.txt"))


(defn parse-inp [text]
  (->> (s/split-lines text)
       (map (fn [line] (->> (s/split line #",")
                            (map #(Integer/parseInt %1)))))))


(def ex-lava-cubes (parse-inp ex-inp))
(def lava-cubes (parse-inp inp))

(- (* 6 (count lava-cubes)) 14)

(defn count-sides-touching [cubes]
  (->> (for [cube cubes
             another-cube cubes
             :when (not= cube another-cube)]
         [cube another-cube])
       (r/map (fn [[cube another-cube]]
                (if (= 1 (r/reduce #(+ %1 (abs %2)) 0 (map - cube another-cube)))
                  1 0)))
       (into [])
       (apply +)))

(defn unconnected-sides [cubes]
  (- (* 6 (count cubes)) (count-sides-touching cubes)))


;; Part 1 
(unconnected-sides ex-lava-cubes) ;; 64
(unconnected-sides lava-cubes) ;; 3494

(defn insp [n] (pp/pprint n) n)

(defn possible-air [[x y z]]
  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

(defn internal-cubes [cubes]
  (let [cube-set (set cubes)
        cube-air (->> cube-set
                      (reduce (fn [acc cube]
                                (->> cube
                                     possible-air
                                     (reduce (fn [acc air] (update acc air #(if (nil? %1) 1 (inc %1)))) acc))) {})
                      (filter (fn [[cube _]] (not (cube-set cube))))
                      (into {}))
        air-set (set (keys cube-air))]
    (->> air-set
         (r/reduce (fn [acc pos]
                     (->> pos
                          (possible-air)
                          (reduce (fn [acc air-pos]
                                    (if (air-set air-pos)
                                      (update acc pos inc)
                                      acc))
                                  acc)))
                   cube-air)
         (r/filter (fn [[_ value]] (= value 6)))
         (into {}))))

(internal-cubes lava-cubes)


(defn internal-cubes [cubes]
  (let [cube-set (set cubes)
        cube-air (->> cube-set
                      (reduce (fn [acc cube]
                                (->> cube
                                     possible-air
                                     (reduce (fn [acc air] (update acc air #(if (nil? %1) 1 (inc %1)))) acc))) {})
                      (filter (fn [[cube _]] (not (cube-set cube))))
                      (into {}))


        air-set (set (keys cube-air))]

(defn external-area [cubes]
  (->> (internal-cubes cubes)
       (* 6)
       (- (unconnected-sides cubes))))

;; Part 2
(external-area ex-lava-cubes) ;; 58
(unconnected-sides lava-cubes) ;; 3494

;; 1598 WRONG TOO LOW
;; 1616 WRONG TOO LOW
;; 3181 TOO HIGH


(- 3494 313)
(- 3494 (* 6 313))


