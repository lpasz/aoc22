(ns aoc22.day-22.monkey-map
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def ex-inp (slurp "../inputs/day-22/ex-inp.txt"))
(def inp (slurp "../inputs/day-22/inp.txt"))


(defn y-boundaries [mmap]
  (->> (group-by first mmap)
       (map (fn [[x vals]]
              (let [ys (map #(second %1) vals)
                    mx (apply max ys)
                    mn (apply min ys)]
                [[x :warp] [mx mn]])))
       (into {})))


(defn side [[x y]]
  (->> {:side-1 [[51 100] [1 50]]
        :side-2 [[101 150] [1 50]]
        :side-3 [[51 100] [51 100]]
        :side-4 [[1 50] [101 150]]
        :side-5 [[51 100] [101 150]]
        :side-6 [[1 50] [151 200]]}
       (filter (fn [[_ [[x1 x2] [y1 y2]]]]
                 (and (<= x1 x x2)
                      (<= y1 y y2))))
       (ffirst)))


(defn add-side [coll]
  (map #(vec (cons (side  %1) %1)) (keys coll)))


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

(def sides {:side-1 [[51 100] [1 50]]
            :side-2 [[101 150] [1 50]]
            :side-3 [[51 100] [51 100]]
            :side-5 [[1 50] [101 150]]
            :side-4 [[51 100] [101 150]]
            :side-6 [[1 50] [151 200]]})

(def side-cubes {:side-1 {:up [:side-6 :left :right :normal]
                          :down [:side-3 :up :down :normal]
                          :left [:side-5 :left :right :reverse]
                          :right [:side-2 :left :right :normal]}
                 :side-2 {:up [:side-6 :down :up :normal]
                          :down [:side-3 :right :left :normal]
                          :left  [:side-1 :right :left :normal]
                          :right [:side-4 :left :left :reverse]}
                 :side-3 {:up [:side-1 :down :up :normal]
                          :down [:side-4 :up :down :normal]
                          :left [:side-5 :up :down :normal]
                          :right [:side-2 :down :up :normal]}
                 :side-4 {:up [:side-3 :down :up :normal]
                          :down [:side-6 :left :left :normal]
                          :left [:side-5 :right :left :normal]
                          :right [:side-2 :right :left :reverse]}
                 :side-5 {:up [:side-3 :left :right :normal]
                          :down [:side-6 :up :down :normal]
                          :right [:side-4 :left :right :normal]
                          :left [:side-1 :left :right :reverse]}
                 :side-6 {:up [:side-5 :down :up :normal]
                          :down [:side-2 :up :down :normal]
                          :left [:side-1 :up :down :normal]
                          :right [:side-4 :down :up :normal]}})

(def rev {:up :down
          :down :up
          :left :right
          :right :left})

(defn warp-to-cube [[x y :as curr-pos] dir borders]
  (let [n  (rem (if (#{:left :right} dir) y x) 50)
        nr (abs (- n 51))
        side (side curr-pos)
        [side-to dir ndir sentido] (get-in side-cubes [side dir])]
    ;; (pp/pprint [curr-pos side side-to dir sentido])
    [ndir (get-in borders [side-to dir (if (= sentido :reverse) nr n)])]))

(warp-to-cube [51 1] :left s)



(rem 51 50)

(defn walk-dir [curr-pos steps dir mmap xb yb borders]
  (if (#{\R \L} steps)
    [curr-pos (shift [dir steps])]
    (loop [curr-pos curr-pos
           steps steps
           dir dir]
      (let [next-pos (walk curr-pos dir)
            next-block (mmap next-pos)
            blocked? (= \# next-block)
            warp? (= nil next-block)
            [wdir warp-to] (warp-to-cube curr-pos dir borders)
            warp-to-block (mmap warp-to)
            warp-blocked? (= \# warp-to-block)]
        (pp/pprint {:next-pos next-pos
                    :next-block next-block
                    :blocked? blocked?})
        (when (and warp? (some? warp-to-block)) (pp/pprint {:warp-to warp-to}))
        (cond
          (zero? steps) [curr-pos dir]
          (and (nil? next-block) warp-blocked?) [curr-pos dir]
          blocked? [curr-pos dir]
          (and warp? (some? warp-to-block)) (recur warp-to (dec steps) wdir)
          :else (recur next-pos (dec steps) dir))))))

(defn walk-the-line [curr-pos direction dirs mmap xb yb borders]
  (loop [curr-pos curr-pos
         direction direction
         dirs dirs]
    ;; (pp/pprint [curr-pos direction])
    (if (empty? dirs)
      [curr-pos direction]
      (let [[steps & dirs] dirs
            [next-pos direction] (walk-dir curr-pos steps direction mmap xb yb borders)]
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

(defn ex2 [text start borders]
  (let [[dirs mmap xb yb] (parse text)
        [[col row] direction] (walk-the-line start
                                             :right
                                             dirs
                                             mmap
                                             xb
                                             yb
                                             borders)]
    (pp/pprint [col row])
    (+ (* 1000 row) (* 4 col) (dir-points direction))))


(parse ex-inp)
(parse inp)


(def sides {:side-1 [[51 100] [1 50]]
            :side-2 [[101 150] [1 50]]
            :side-3 [[51 100] [51 100]]
            :side-5 [[1 50] [101 150]]
            :side-4 [[51 100] [101 150]]
            :side-6 [[1 50] [151 200]]})

(def ex-sides {:side-1 [[9 12] [1 4]]
               :side-2 [[1 4] [5 8]]
               :side-3 [[5 8] [5 8]]
               :side-5 [[9 12] [5 8]]
               :side-4 [[9 12] [9 12]]
               :side-6 [[9  12] [13 15]]})


(defn border [v]
  (let [[x1 x2] (first v)
        [y1 y2] (second v)]
    {:up (into {} (map-indexed (fn [idx x]    [(inc idx) [x y1]]) (range x1 (inc x2))))
     :down (into {}  (map-indexed (fn [idx x] [(inc idx) [x y2]]) (range x1 (inc x2))))
     :left (into {}  (map-indexed (fn [idx y] [(inc idx) [x1 y]]) (range y1 (inc y2))))
     :right (into {} (map-indexed (fn [idx y] [(inc idx) [x2 y]]) (range y1 (inc y2))))}))



(def s (into {} (map (fn [[k v]] [k (border v)]) sides)))

;; (ex1 ex-inp [9 1]) ;; 6032

;too high 109046
;; too high 150158
(ex2 inp [51 1] s)

