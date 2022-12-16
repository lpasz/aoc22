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
       (map (fn [[valve flow-rate tunnels-to]] [(keyword valve) {:valve (keyword valve)
                                                                 :flow-rate (Integer/parseInt flow-rate)
                                                                 :tunnels-to (map keyword (s/split tunnels-to #", "))}]))
       (into (sorted-map))))

(defn next-valves [[curr-time current-valve curr-open curr-visited curr-pressure] valves]
  (->>  (valves current-valve)
        :tunnels-to
        (map valves)
        (mapcat (fn [{:keys [valve flow-rate]}]
                  (cond
                    (zero? flow-rate) [[(dec curr-time)
                                        valve
                                        curr-open
                                        (conj curr-visited valve)
                                        curr-pressure]]
                    (curr-open valve) [[(dec curr-time)
                                        valve
                                        curr-open
                                        (conj curr-visited valve)
                                        curr-pressure]]
                    :else [;; go to valve and open it
                           [(- curr-time 2)
                            valve
                            (conj curr-open valve)
                            (conj curr-visited valve)
                            (+ flow-rate curr-pressure)]
                          ;; go to valve
                           [(dec curr-time)
                            valve
                            curr-open
                            (conj curr-visited valve)
                            curr-pressure]])))
        (filter (fn [[time]] (> time 0)))))

(def evalves (parse-inp ex-inp))

(next-valves [30 :AA #{} #{} 0] evalves)

(defn shortest-path [valves start]
  (loop [stack #{[30 start #{} #{} 0]}
         max-pressure 0]
    (let [current (first stack)
          time (first current)
          pressure (peek current)]
      (cond (empty? stack) max-pressure
            (zero? time) (recur (disj stack current)
                                (max max-pressure pressure))
            :else (recur (into #{} (concat (disj stack current)
                                           (next-valves current valves)))
                         max-pressure)))))

(shortest-path evalves :AA)

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