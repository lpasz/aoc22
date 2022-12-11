(ns aoc22.day-11.monkey-in-the-middle
  (:require [clojure.string :as s]))

(def inp (slurp "src/day-11/inp.txt"))
(def ex-inp (slurp "src/day-11/ex-inp.txt"))

(defn to-int [i] (bigdec i))

(defn parse-monkey-number [monkey-number]
  (Integer/parseInt (s/replace monkey-number #"[^0-9]" "")))

(defn parse-starting-items [starting-items]
  (->> (s/split (last (s/split starting-items #": ")) #", ")
       (map to-int)
       (into [])))

(parse-starting-items "  Starting items: 79, 98")

(map to-int (s/split (last (s/split "  Starting items: 79, 98" #": ")) #", "))

(defn parse-operation [operation]
  (-> (str "(fn [old] ("
           (s/replace operation #"Operation: new = old " "")
           " old ))")
      (read-string)
      (eval)))

(defn parse-test [test]
  (let [tests (-> (apply str test) (s/replace #"[^0-9]+" ",") (s/split #","))
        [divisible-for m1 m2] (->> tests (drop 1) (map to-int))]
    {:test #(if (zero? (rem % divisible-for)) m1 m2)
     :divisible-for divisible-for
     true m1
     false m2}))

(defn parse-line [line]
  (let [[monkey-number starting-items operation & test] (s/split-lines line)]
    [(parse-monkey-number monkey-number)
     (merge {:items (parse-starting-items starting-items)
             :operation (parse-operation operation)}
            (parse-test test))]))

(defn parse-inp [text]
  (->> (s/split text #"\n\n")
       (map parse-line)
       (into (sorted-map))))

(defn mturn [monkeys relief]
  (reduce (fn [smonkeys monkey-num]
            (->> (get-in smonkeys [monkey-num :items])
                 (reduce (fn [rmonkeys P-item]
                           (let [worry-lvl ((get-in smonkeys [monkey-num :operation]) P-item)
                                 worry-lvl (quot worry-lvl relief)
                                 next-monkey  ((get-in smonkeys [monkey-num :test]) worry-lvl)]
                             (-> rmonkeys
                                 (update-in [monkey-num :inspected] #(if (nil? %) 1 (inc %)))
                                 (update-in [monkey-num :items] #(drop 1 %))
                                 (update-in [next-monkey :items] #(concat % [worry-lvl])))))
                         smonkeys)))
          monkeys
          (keys monkeys)))

(defn monkey-turn [text turns relief]
  (let [monkeys (parse-inp text)
        denominator (->> monkeys
                         (map #(:divisible-for (second %)))
                         (reduce *))]
    (->> (loop [n 0
                monkeys monkeys]
           (if (= n turns)
             monkeys
             (recur (inc n)
                    (mturn monkeys relief))))
         (map (fn [[n m]] (get m :inspected)))
         (sort >)
         (take 2)
         (apply *))))

(defn ex1 [text]
  (monkey-turn text 20 3))

(defn ex2
  ([text n]  (monkey-turn text n 1))
  ([text]  (monkey-turn text 10000 1)))

(new java.utils.ArrayList (parse-inp ex-inp))

(ex1 inp)    ;; 55458
(ex1 ex-inp) ;; 10605

(ex2 inp 500)
