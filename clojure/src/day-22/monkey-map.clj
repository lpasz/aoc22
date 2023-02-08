(ns aoc22.day-22.monkey-map
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def ex-inp (slurp "lib/day-22/ex-inp.txt"))
(def inp (slurp "../inputs/day-22/inp.txt"))

(defn y-boundaries [mmap]
  (->> (group-by ffirst mmap)
       (map (fn [[x vals]]
              (let [ys (map #(second (first %1)) vals)
                    mx (apply max ys)
                    mn (apply min ys)]
                [[x :warp] [mx mn]])))
       (into {})))

(defn x-boundaries [mmap]
  (->> (group-by #(second (first %1)) mmap)
       (map (fn [[y vals]]
              (let [xs (map ffirst vals)
                    mx (apply max xs)
                    mn (apply min xs)]
                [[:warp y] [mx mn]])))
       (into {})))

(defn parse-mmap [text]
  (->> (s/split-lines text)
       (keep-indexed (fn [idy line]
                       (->> line
                            (keep-indexed (fn [idx itm] (if (not= \space itm)
                                                          [[(inc idx) (inc idy)] itm]))))))
       (mapcat identity)
       (into (sorted-map))))

(defn parse-dir [dirs]
  (->> (partition-by #{\R \L} (seq dirs))
       (map (fn [[num :as nums]] (if (#{\R \L} num)
                                   num
                                   (Integer/parseInt (apply str nums)))))
       (flatten)))

(defn parse [text]
  (let [[mmap mdir] (s/split text #"\n\n")
        mmap (parse-mmap mmap)]
    [(parse-dir (s/replace mdir "\n" ""))
     mmap
     (x-boundaries mmap)
     (y-boundaries mmap)]))

(def shift {[:right \R] :down
            [:right \L] :up
            [:left \R] :up
            [:left \L] :down
            [:up \R] :right
            [:up \L] :left
            [:down \R] :left
            [:down \L] :right})

(defn walk [[x y] dir]
  (cond (= dir :right) [(inc x)      y]
        (= dir :left)  [(dec x)      y]
        (= dir :up)    [x       (dec y)]
        (= dir :down)  [x       (inc y)]))

(defn warp-x [[x y] xb]
  (first (filter #(not= x %1) (xb [:warp y]))))

(defn warp-y [[x y] yb]
  (first (filter #(not= y %1) (yb [x :warp]))))


(defn warp-to [[x y :as curr-pos] dir xb yb]
  (if (#{:left :right} dir)
    [(warp-x curr-pos xb) y]
    [x (warp-y curr-pos yb)]))

(defn walk-dir [curr-pos steps dir mmap xb yb]
  (if (#{\R \L} steps)
    [curr-pos (shift [dir steps])]
    (loop [curr-pos curr-pos
           steps steps
           dir dir]
      (let [next-pos (walk curr-pos dir)
            next-block (mmap next-pos)
            blocked? (= \# next-block) 
            warp? (= nil next-block)
            warp-to (warp-to curr-pos dir xb yb)
            warp-to-block (mmap warp-to)
            warp-blocked? (= \# warp-to-block)]
        ;; (pp/pprint {:next-pos next-pos
        ;;             :next-block next-block
        ;;             :warp? warp?
        ;;             :warp-to warp-to
        ;;             :warp-to-block warp-to-block
        ;;             :blocked? blocked?
        ;;             :warp-blocked? warp-blocked?})
        (cond
          (zero? steps) [curr-pos dir]
          (and (nil? next-block) warp-blocked?) [curr-pos dir]
          blocked? [curr-pos dir]
          (and warp? (some? warp-to-block)) (recur warp-to (dec steps) dir)
          :else (recur next-pos (dec steps) dir))))))

(defn walk-the-line [curr-pos direction dirs mmap xb yb]
  (loop [curr-pos curr-pos
         direction direction
         dirs dirs]
    (if (empty? dirs)
      [curr-pos direction]
      (let [[steps & dirs] dirs
            [next-pos direction] (walk-dir curr-pos steps direction mmap xb yb)]
        (recur next-pos direction dirs)))))

(def dir-points {:right 0
                 :down 1
                 :left 2
                 :up 3})

(defn ex1 [text start]
  (let [[dirs mmap xb yb] (parse text)
        [[col row] direction] (walk-the-line start
                                             :right
                                             dirs
                                             mmap
                                             xb
                                             yb)]
    (pp/pprint [col row])
    (+ (* 1000 row) (* 4 col) (dir-points direction))))


(parse ex-inp)

(ex1 ex-inp [9 1]) ;; 6032
(ex1 inp [51 1])

