(ns aoc2018.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (str/split-lines (slurp "resources/day3input")))

(def example ["#1 @ 1,3: 4x4"
              "#2 @ 3,1: 4x4"
              "#3 @ 5,5: 2x2"])

(defn claim->coords [claim]
  (let [[id x y x-size y-size] (map #(Long/parseLong %) (rest (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)))]
    [id (for [x' (range x-size)
              y' (range y-size)]
          [(+ x x') (+ y y')])]))

(comment
  (claim->coords "#3 @ 5,5: 2x2")

  (->> example
       (map (comp second claim->coords))
       (reduce concat)
       (frequencies)
       (filter (comp #(> % 1) second))
       (count))

  (time (->> input
             (mapcat (comp second claim->coords))
             (frequencies)
             (filter (comp #(> % 1) second))
             (count))))

(comment
  "Part 2"
  (time
   (let [ones (->> input
                   (mapcat (comp second claim->coords))
                   (frequencies)
                   (filter (comp #(= % 1) second))
                   (map first)
                   set)]
     (some #(when (set/subset? (set (second %)) ones)
              (first %)) (map claim->coords input)))))
