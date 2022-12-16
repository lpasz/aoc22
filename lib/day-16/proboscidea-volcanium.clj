(ns aoc22.day-16.proboscidea-volcanium
  (:require [clojure.string :as s]))

(def inp (slurp "lib/day-16/inp.txt"))
(def ex-inp (slurp "lib/day-16/ex-inp.txt"))

(defn parse-inp [text]
  (->> (s/split-lines text)
       (mapcat #(s/split % #"(Valve | has flow rate=|; tunnels lead to valves |; tunnel leads to valve )"))
       (filter not-empty)
       (partition 3)
       (map (fn [[valve flow-rate points-to]] [valve {:valve valve
                                                      valve (s/split points-to #", ")
                                                      :flow-rate (Integer/parseInt flow-rate)
                                                      :points-to (s/split points-to #", ")}]))
       (into (sorted-map))))

(defn insp [x]
  (clojure.pprint/pprint x)
  x)

(defn next-valves [current valves curr-time curr-open visited]
  (->> (:points-to (valves current))
       (map valves)
       (filter #(not (visited (:valve %))))
       (mapcat (fn [{:keys [valve flow-rate]}]
                 (cond
                   (zero? flow-rate) [[(inc curr-time) valve curr-open (conj visited valve)]]
                   (curr-open valve) [[(inc curr-time) valve curr-open (conj visited valve)]]
                   :else [;; move and open the valve
                          [(+ curr-time 2) valve (conj curr-open valve) (conj visited valve)]
                          ;; not open the valve, just move
                          ;;[(inc curr-time) valve curr-open]
                          ])))
       (reduce (fn [[times-up keep-going] [time valve open visited]]
                 (if (= time 30)
                   [(conj times-up open) keep-going]
                   [times-up (conj keep-going [time valve open visited])]))
               [[] []])))

(def evalves (parse-inp ex-inp))

(reduce (fn [acc [k {:keys [flow-rate]}]]
          (+ acc flow-rate)) 0 evalves)

(next-valves "AA" evalves 0 #{} #{})

(defn shortest-path [valves start time]
  (loop [stack [[time start #{} #{}]]
         open []]
    (if (empty? stack)
      open
      (let [[time valve open-valves visited] (first stack)
            stack (rest stack)
            [times-up keep-going] (next-valves valve valves time open-valves visited)
            stack (if (not-empty keep-going) (concat stack keep-going) stack)
            paths-to-end (cond (not-empty times-up) (conj open times-up)
                               (empty keep-going) (conj open open-valves)
                               :else open)]
        (recur stack paths-to-end)))))

(let [valves-flow-rate (into {} (map (fn [[k v]] [k (:flow-rate v)]) evalves))]
  (->> (shortest-path evalves "AA" 0)
       (map (fn [xs] (reduce (fn [acc x] (+ acc (valves-flow-rate x))) 0 xs)))
       (sort >)))

;;   (clojure.pprint/pprint {:valves valves})
;;   (loop [{:keys [valve flow-rate points-to] :as vvalve} (valves "AA")
;;          valves valves
;;          open #{}
;;          visited #{}
;;          minutes 30]
;;     (clojure.pprint/pprint {:vvalve vvalve
;;                             :visited visited
;;                             :open open
;;                             :minutes minutes})
;;     (cond
;;       ;; time's up
;;       (zero? minutes)
;;       open

;;       ;; move along to next node
;;       (or (open valve) (zero? flow-rate))
;;       (let [next-valve (next-valve points-to valves visited)
;;             next-valve (valves next-valve)]
;;         (recur next-valve
;;                valves
;;                open
;;                (conj visited valve)
;;                (dec minutes)))

;;       ;; open current node
;;       (pos? flow-rate)
;;       (recur vvalve
;;              valves
;;              (conj open valve)
;;              visited
;;              (dec minutes)))))



