(ns aoc2018.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [ubergraph.core :as uber]))

(def example
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defn parse-step [line]
  (let [[_ from to] (re-matches
                     #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
                     line)]
    [from to]))

(def graph (apply uber/digraph (map parse-step (str/split-lines example))))

(defn roots [graph]
  (set/difference (set (uber/nodes graph))
                  (set (map :dest (uber/edges graph)))))

(defn all-parents-open? [graph open query]
  (set/subset? (set (uber/predecessors graph query)) (set open)))

(defn reverse-difference
  "To keep the pipe in the next function nice and clean."
  [s1 s2]
  (set/difference s2 s1))

(defn candidates [graph open]
  (->> open
       (mapcat #(uber/find-edges graph {:src %}))
       (map :dest)
       (concat (roots graph))
       (set)
       (reverse-difference (set open))
       (filter (partial all-parents-open? graph open))
       (sort)))

(defn order
  ([graph] (order graph []))
  ([graph open]
   (if (= (count open) (uber/count-nodes graph))
     open
     (recur graph (conj open (first (candidates graph open)))))))

(comment
  (order graph [])
  (def graph2 (apply uber/digraph (map parse-step (str/split-lines (slurp "resources/day7input")))))
  (uber/pprint graph2)
  (order graph2))
