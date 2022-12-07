(ns aoc22.day-07.no-space-left-on-device
  (:require [clojure.string :as s]))

(def ex-inp (slurp "src/day-07/ex-inp.txt"))
(def inp (slurp "src/day-07/inp.txt"))

(defn parse-line [line]
  (cond (s/starts-with? line "cd") (s/replace line #"(cd.|\n)" "")
        (s/starts-with? line "ls") (->> (s/split line #"[^0-9]")
                                        (reduce #(if (not-empty %2)
                                                   (+ %1 (Integer/parseInt %2))
                                                   %1)
                                                0))))

(defn parse-cmds [text]
  (->> (s/split text #"\$ ")
       (keep not-empty)
       (map parse-line)))

(defn increase-folders-size [fs cd cmd]
  (loop [cd cd 
         fs fs]
    (if (not-empty cd)
      (recur (drop-last cd)
             (update fs (s/join "/" cd) #(if (nil? %)
                                           cmd
                                           (+ cmd %))))
      fs)))

(defn return-one-folder [full-path] (into [] (drop-last full-path)))
(defn enter-in-folder [full-path new-folder] (conj full-path new-folder))

(defn directory [text]
  (->> (parse-cmds text)
       (reduce (fn [[full-path dir-structure] cmd]
                 (cond
                   (number? cmd) [full-path (increase-folders-size dir-structure full-path cmd)]
                   (= ".." cmd)  [(return-one-folder full-path) dir-structure]
                   (string? cmd) [(enter-in-folder full-path cmd) dir-structure]))
               [[] {}])
       (second)))

(defn ex1 [text]
  (->> (directory text)
       (vals)
       (filter number?)
       (filter #(>= 100000 %))
       (apply +)))

(def total-space 70000000)
(def space-req 30000000)

(defn smallest-file-size-to-delete [files-size min-required-size]
  (->> files-size
       (filter #(> % min-required-size))
       (sort)
       (first)))


(defn ex2 [text]
  (let [directory (directory text)
        used-space (directory "/")
        remaining-space (- total-space used-space)
        need-delete-at-least (- space-req remaining-space)]
    (->> (vals directory)
         (smallest-file-size-to-delete need-delete-at-least))))

(ex1 ex-inp)
(ex1 inp)
(ex2 ex-inp)
(ex2 inp)



