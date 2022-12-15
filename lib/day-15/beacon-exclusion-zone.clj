(ns aoc22.day-15.beacon-exclusion-zone
  (:require [clojure.string :as s]
            [clojure.parallel :as p]))

(def inp (slurp "lib/day-15/inp.txt"))
(def ex-inp (slurp "lib/day-15/ex-inp.txt"))

(defn where-is-not-fn [[sx sy] [bx by]]
  (let [x-diff (abs (- sx bx))
        y-diff (abs (- sy by))]
    (fn [[tx ty]]
      (> (+ x-diff y-diff 1) (+ (abs (- sx tx)) (abs (- sy ty)))))))

(defn where-is-not [sb]
  (->> (partition 2 sb)
       (reduce (fn [acc [s-pos b-pos]]
                 (conj acc (where-is-not-fn s-pos b-pos))) [])))

(defn parse-inp [text]
  (->> (s/split-lines text)
       (mapcat #(s/split % #"(Sensor at x=|, y=|: closest beacon is at x=|, y=)"))
       (filter not-empty)
       (map #(Integer/parseInt %))
       (partition 2)))

(def funn ((->> (parse-inp ex-inp) where-is-not) [8 7]))

(defn ex1 [text y]
  (let [sensors-and-beacons (parse-inp text)
        beacons (->> sensors-and-beacons (partition 2) (map second) (into #{}))
        wnots (where-is-not sensors-and-beacons)
        max-x (->> sensors-and-beacons (map first) (apply max) (+ y))
        min-x (->> sensors-and-beacons (map first) (apply min) (+ (- y)))]
    (->> (range min-x (inc max-x))
         (keep (fn [x] (if (not (beacons [x y]))
                         (some (fn [wnot] (wnot [x y])) wnots))))
         (count))))

(ex1 ex-inp 10) ;; 26
;; too slow
;; (ex1 inp 2000000) ;; 5525847

;; (defn ex2 [text [min-x max-x] [min-y max-y]]
;;   (let [sensors-and-beacons (parse-inp text)
;;         wnots (where-is-not sensors-and-beacons)]
;;     (->> (range min-x (inc max-x))
;;          (filter (fn [x]
;;                    (->> (range min-y (inc max-y))
;;                         (filter (fn [y]
;;                                   (->> wnots
;;                                        (some (fn [wnot]
;;                                                (wnot [x y]))))))
;;                         (not-empty)))))))




(defn ex2 [text [mnx mxx] [mny mxy]]
  (let [rg (for [x (range mnx (inc mxx)) y (range mny (inc mxy))] [x y])
        sensors-and-beacons (parse-inp text)
        wnots (where-is-not sensors-and-beacons)]
    (->>  rg
          (filter (fn [pos] 
                    (every? (fn [wnot] (false? (wnot pos))) wnots)))
          (first))))

(ex2 ex-inp [0 20] [0 20])
(ex2 inp [0 4000000] [1 10])

(s/split-lines ex-inp)
