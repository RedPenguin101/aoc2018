(ns aoc2018.day1norvig
  (:require [aoc2018.aoc-utils :refer [get-input]]))

(def input (get-input 1 #(Long/parseLong %)))

(time (reduce + input))
;; => 578

(defn first-dupe [seen item] (if (seen item) (reduced item) (conj seen item)))

(time (reduce first-dupe #{} (reductions + (cycle input))))
;; => 82516
