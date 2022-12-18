(ns aoc22.day-16.proboscidea-volcanium
  (:require [clojure.string :as s]
            [clojure.core.reducers :as r]))

(def inp (slurp "lib/day-16/inp.txt"))
(def ex-inp (slurp "lib/day-16/ex-inp.txt"))

(defn parse-inp [text]
  (->> (s/split-lines text)
       (mapcat #(s/split % #"(Valve | has flow rate=|; tunnels lead to valves |; tunnel leads to valve )"))
       (filter not-empty)
       (partition 3)
       (mapcat (fn [[valve flow-rate tunnels-to]]
                 (if (= flow-rate "0")
                   [[(keyword valve) {:flow-rate (Integer/parseInt flow-rate)
                                      :tunnels-to (mapv keyword (s/split tunnels-to #", "))}]]
                   [[(keyword valve) {:flow-rate 0
                                      :tunnels-to (conj (mapv keyword (s/split tunnels-to #", ")) (keyword (s/lower-case valve)))}]
                    [(keyword (s/lower-case valve)) {:flow-rate (Integer/parseInt flow-rate)
                                                     :tunnels-to [(keyword valve)]}]])))
       (into (sorted-map))))


(def exvalves (parse-inp ex-inp))
(def valves (parse-inp inp))

;;0     2*     3    4*     6     7    8*    10    11    12    13    14    15    16*   18    19    20*   22    23*  (all valves that can be open are open after this point)
;;AA -> DD -> CC -> BB -> AA -> II -> JJ -> II -> AA -> DD -> EE -> FF -> GG -> HH -> GG -> FF -> EE -> DD -> CC

;;AA  <-+           <-+
;;|    |             |
;;II  BB <-> CC <-> DD <-> EE <-> FF <-> GG <-> HH
;;|
;;JJ


;;Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
;;Valve BB has flow rate=13; tunnels lead to valves CC, AA
;;Valve CC has flow rate=2; tunnels lead to valves DD, BB
;;Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
;;Valve EE has flow rate=3; tunnels lead to valves FF, DD
;;Valve FF has flow rate=0; tunnels lead to valves EE, GG
;;Valve GG has flow rate=0; tunnels lead to valves FF, HH
;;Valve HH has flow rate=22; tunnel leads to valve GG
;;Valve II has flow rate=0; tunnels lead to valves AA, JJ
;;Valve JJ has flow rate=21; tunnel leads to valve II


;;+------------------------+
;;|                        |
;;AA<---------+            |
;;|          |             |
;;II        BB <-> CC <-> DD <-> EE <-> FF <-> GG <-> HH <-> 22
;;|         |      |      |      |
;;JJ        13     2     20      3
;;|
;;21


(defn shortest-path  [graph start end]
  (loop [stack [[0 start]]
         prev-visited #{}
         min-dist 10000000]
    (let [[dist curr-pos] (first stack)
          rstack (rest stack)]
      (cond
        (empty? stack) min-dist
        (= curr-pos end) (recur rstack prev-visited (min dist min-dist))
        :else (let [upstack (if (and (< dist min-dist) (not (prev-visited curr-pos)))
                              (->> graph
                                   (curr-pos)
                                   (:tunnels-to)
                                   (map (fn [t] [(inc dist) t]))
                                   (concat rstack))
                              rstack)]
                (recur upstack (conj prev-visited curr-pos) min-dist))))))

(shortest-path exvalves :AA :bb)

(defn interest-point [graph]
  (keep (fn [[k v]] (if (> (:flow-rate v) 0)
                      k)) graph))

(interest-point exvalves)

(defn presure-released [open graph]
  (reduce (fn [acc [started valve]]
            (+ acc (* started (:flow-rate (valve graph)))))
          0 open))

(presure-released #{[1 :bb]} exvalves)

(defn from-key-to-key [start graph]
  (let [ip (interest-point graph)
        ipa (conj ip start)]
    (into {} (for [from ipa to ipa :when (not= from to)]
               [[from to] (shortest-path graph from to)]))))

(defn from-key-to-key-p [start graph]
  (let [ip (interest-point graph)
        ipa (conj ip start)]
    (into {} (for [from ipa to ipa :when (not= from to)]
               [[from to]  (* (:flow-rate (to graph)) (- 26 (shortest-path graph from to)))]))))

(into (sorted-map) (from-key-to-key-p :AA valves))

(defn most-steam [graph start-at vist timer]
  (let [ip (interest-point graph)
        from-to (from-key-to-key start-at graph)]
    (loop [stack [[(dec timer) start-at vist 0]]
           max-pressure-release 0
           paths #{}]
      (let [[time-rem curr-pos vist pressure] (first stack)
            rstack (rest stack)
            vist (conj vist curr-pos)
            next-poss (filter #(not (vist %1)) ip)]
        (cond
          (empty? stack)  paths
          (zero? time-rem) (recur rstack (max max-pressure-release pressure) (conj paths [(max max-pressure-release pressure) vist]))
          (empty? next-poss) (recur rstack (max max-pressure-release pressure) (conj paths [(max max-pressure-release pressure) vist]))
          :else (let [upstack (->> next-poss
                                   (r/map (fn [next-pos]
                                            (let [time-to-next (from-to [curr-pos next-pos])
                                                  time-rem (- (+ 1 time-rem) time-to-next)]
                                              (if (neg? time-rem)
                                                [0 curr-pos vist pressure]
                                                [time-rem
                                                 next-pos
                                                 vist
                                                 (+ pressure (* time-rem (:flow-rate (next-pos graph))))]))))
                                   (into #{})
                                   (concat rstack))]
                  (recur upstack max-pressure-release paths)))))))

(most-steam valves :AA #{} 26)

(r/reduce (fn [max-steam [steam-me start-visited]]
            (->> (most-steam valves :AA start-visited 26)
                 (r/reduce (fn [round-max-steam [steam-elephant _vist]]
                             (let [our-round-max-steam (+ steam-me steam-elephant)]
                               (max our-round-max-steam round-max-steam))) 0)
                 (max max-steam)))
          0
          (most-steam valves :AA #{} 26))
;; 2135 wrong too low


(defn most-steam-for-2 [graph start-at timer]
  (let [ip (interest-point graph)
        from-to (from-key-to-key start-at graph)]
    (loop [stack [[[(dec timer) (dec timer)] start-at start-at #{} 0]]
           max-pressure-release 0]
      (let [[[time-rem1 time-rem2] curr-pos1 curr-pos2 vist pressure] (first stack)
            rstack (rest stack)
            vist (conj vist curr-pos1 curr-pos2)
            next-poss (filter #(not (vist %1)) ip)]
        (cond
          (empty? stack) max-pressure-release
          (and (zero? time-rem1) (zero? time-rem2)) (recur rstack (max max-pressure-release pressure))
          (empty? next-poss) (recur rstack (max max-pressure-release pressure))
          :else (let [upstack (->> (for [next-pos1 next-poss
                                         next-pos2 next-poss
                                         :when (not= next-pos1 next-pos2)]
                                     [next-pos1 next-pos2])
                                   (r/map (fn [[next-pos1 next-pos2]]
                                            [next-pos1
                                             next-pos2
                                             (- (+ 1 time-rem1) (from-to [curr-pos1 next-pos1]))
                                             (- (+ 1 time-rem2) (from-to [curr-pos2 next-pos2]))]))
                                   (r/reduce (fn [acc [n1 n2 t1 t2]]
                                               (if (and (not (acc [n1 n2 t1 t2]))
                                                        (not (acc [n2 n1 t2 t1])))
                                                 (conj acc [n2 n1 t2 t1])
                                                 acc)) #{})
                                   (r/map (fn [[next-pos1 next-pos2 t1 t2]]
                                            (cond
                                              (and (neg? t1) (neg? t2))
                                              [[0 0] curr-pos1
                                               next-pos2
                                               vist
                                               pressure]

                                              (neg? t1)
                                              [[0 t2]
                                               curr-pos1
                                               next-pos2
                                               vist
                                               (+ pressure (* t2 (:flow-rate (next-pos2 graph))))]

                                              (neg? t2)
                                              [[t1 0]
                                               next-pos1
                                               curr-pos2
                                               vist
                                               (+ pressure (* t1 (:flow-rate (next-pos1 graph))))]

                                              :else
                                              [[t1 t2]
                                               next-pos1
                                               next-pos2
                                               vist
                                               (+ (+ pressure (* t1 (:flow-rate (next-pos1 graph))))
                                                  (* t2 (:flow-rate (next-pos2 graph))))])))
                                   (into #{})
                                   (concat rstack)
                                   (r/reduce (fn [acc [[t1 t2] n1 n2 vist pressure :as me-and-elefant]]
                                               (if (and (not (acc [[t1 t2] n1 n2 vist pressure]))
                                                        (not (acc [[t2 t1] n2 n1 vist pressure])))
                                                 (conj acc me-and-elefant)
                                                 acc)) #{})
                                   (into #{}))]
                  (recur upstack max-pressure-release)))))))

(time (most-steam exvalves :AA 30))       ;; 1651
;; (time (most-steam valves :AA 30))         ;; 1595
(time (most-steam-for-2 exvalves :AA 26)) ;; 1707
(time (most-steam-for-2 valves :AA 26))   ;; 1707 
;; 2027 too low
