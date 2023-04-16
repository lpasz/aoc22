(ns aoc22.day-23.unstable-diffusion
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(defn draw-board [[inp order]]
  (let [min-x (->> inp (map first) (apply min) dec)
        max-x (->> inp (map first) (apply max) inc)
        min-y (->> inp (map second) (apply min) dec)
        max-y (->> inp (map second) (apply max) inc)]
    (->> (range min-y (inc max-y))
         (map (fn [y]
                (->> (range min-x (inc max-x))
                     (map (fn [x]
                            (if (get inp [x y])
                              \#
                              \.)))
                     (apply str))))
         (map println)
         (doall))
    (println)
    [inp order]))

(defn insp [n]
  (pp/pprint n)
  n)

(def mini-ex-inp (str ".....\n"
                      "..##.\n"
                      "..#..\n"
                      ".....\n"
                      "..##.\n"
                      "....."))

(def ex-inp (slurp "../inputs/day-23/ex-inp.txt"))
(def inp-txt (slurp "../inputs/day-23/inp.txt"))

(def order '(:n :s :w :e))

(defn parse [text]
  (->> (s/split-lines text)
       (map-indexed (fn [idy line]
                      (->> line
                           (map-indexed (fn [idx itm]
                                          [[(inc idx) (inc idy)] itm])))))
       (mapcat identity)
       (filter #(= (second %) \#))
       (map first)
       (set)))

(def mini (parse mini-ex-inp))
(def ex (parse ex-inp))
(def inp (parse inp-txt))

(def surrounding {:n [0 -1]
                  :ne [+1 -1]
                  :e [+1 0]
                  :se [+1 +1]
                  :s [0 +1]
                  :sw [-1 +1]
                  :w [-1 0]
                  :nw [-1 -1]})

(defn go-to [[x y] dir]
  (let [[dx dy] (get surrounding dir)]
    [(+ x dx) (+ y dy)]))

(defn around [[x y] inp]
  (->> surrounding
       (map (fn [[d [xs ys]]] [d [(+ xs x) (+ ys y)]]))
       (filter #(get inp (second %)))
       (into {})))

(def directions {:n [:nw :n :ne]
                 :e [:ne :e :se]
                 :w [:nw :w :sw]
                 :s [:sw :s :se]})

(defn rotate [coll]
  (let [head (first coll)
        tail (rest coll)]
    (concat tail (list head))))

(defn first-or [deft coll]
  (if (empty? coll)
    deft
    (first coll)))


(defn first-valid-move-in-order [xy around order]
  (->> order
       (keep (fn [dir]
               (when (empty? (select-keys around (dir directions)))
                 [xy (go-to xy dir)])))
       (first-or [xy xy])))

(defn propose-move [[inp order]]
  (->> inp
       (map (fn [xy]
              (let [around (around xy inp)]
                (if (empty? around)
                  [xy xy]
                  (first-valid-move-in-order xy around order)))))))

(defn remove-move-with-same-target [inp]
  (->> inp
       (group-by second)
       (mapcat (fn [[_ v]]
                 (if (= (count v) 1)
                   [(second (first v))]
                   (map first v))))
       (into #{})))


(defn move [[inp order]]
  [(remove-move-with-same-target (propose-move [inp order])) (rotate order)])


(defn calc-board-empty-squares [[inp _]]
  (let [min-x (->> inp (map first) (apply min))
        max-x (->> inp (map first) (apply max))
        min-y (->> inp (map second) (apply min))
        max-y (->> inp (map second) (apply max))]
    (->> (range min-y (inc max-y))
         (mapcat (fn [y]
                 (->> (range min-x (inc max-x))
                      (keep (fn [x] (when (nil? (get inp [x y])) :empty))))))
         (count))))

(defn do-n-moves [[inp n-m]]
  (reduce (fn [acc _] (move acc)) [inp n-m] (range 10)))

(->> [inp order]
     do-n-moves
     calc-board-empty-squares)

