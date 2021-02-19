(ns aoc2018.day2norvig
  (:require [aoc2018.aoc-utils :refer [get-input quantify keep']]))

(def input (get-input 2))

(let [num-with (fn [n] (fn [xs] (some #{n} (vals (frequencies xs)))))]
  (* (quantify (num-with 2) input) (quantify (num-with 3) input)))
;; => 7163

(let [common (fn [A B] (apply str (keep' #(when (= %1 %2) %1) A B)))]
  (-> (for [a input b input :when (= 1 (- (count a) (count (common a b))))] (common a b)) first time))
;; => "ighfbyijnoumxjlxevacpwqtr"
