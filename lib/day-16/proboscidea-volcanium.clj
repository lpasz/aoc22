(ns aoc22.day-16.proboscidea-volcanium
  (:require [clojure.string :as s]))

(defn insp [n]
  (clojure.pprint/pprint n)
  n)

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

(def evalves (parse-inp ex-inp))

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

(shortest-path evalves :AA :bb)

(defn interest-point [graph]
  (keep (fn [[k v]] (if (> (:flow-rate v) 0)
                      k)) graph))

(interest-point evalves)

(defn presure-released [open graph]
  (reduce (fn [acc [started valve]]
            (+ acc (* started (:flow-rate (valve graph)))))
          0 open))

(presure-released #{[1 :bb]} evalves)

(defn from-key-to-key [start graph]
  (let [ip (interest-point graph)
        ipa (conj ip start)]
    (into {} (for [from ipa to ipa :when (not= from to)]
               [[from to] (shortest-path graph from to)]))))

(defn most-steam [graph start-at timer]
  (let [ip (interest-point graph)
        from-to (from-key-to-key start-at graph)]
    (loop [stack [[(dec timer) start-at #{} 0]]
           max-pressure-release 0]
      (let [[time-rem curr-pos vist pressure] (first stack)
            rstack (rest stack)
            vist (conj vist curr-pos)
            next-poss (filter #(not (vist %1)) ip)]
        (cond
          (empty? stack) max-pressure-release
          (zero? time-rem) (recur rstack (max max-pressure-release pressure))
          (empty? next-poss) (recur rstack (max max-pressure-release pressure))
          :else (let [upstack (->> next-poss
                                   (pmap (fn [next-pos]
                                           (let [time-to-next (from-to [curr-pos next-pos])
                                                 time-rem (- (+ 1 time-rem) time-to-next)]
                                             (if (neg? time-rem)
                                               [0 curr-pos vist pressure]
                                               [time-rem
                                                next-pos
                                                vist
                                                (+ pressure (* time-rem (:flow-rate (next-pos graph))))]))))
                                   (filter #(< max-pressure-release (peek %)))
                                   (concat rstack))]
                  (recur upstack max-pressure-release)))))))

(def evalves (parse-inp ex-inp))
(def evalves (parse-inp inp))

(time (most-steam (parse-inp ex-inp) :AA 30)) ;; 1651
(time (most-steam (parse-inp inp) :AA 30)) ;; 1595
