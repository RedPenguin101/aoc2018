(ns aoc2018.aoc-utils
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn- input-template [day]
  (str "resources/day" day "input"))

(defn integer-parse [str]
  (mapv #(Long/parseLong %) (re-seq #"\d+" str)))

(defn get-input
  ([day] (get-input day identity))
  ([day parser]
   (map parser (str/split-lines (slurp (input-template day))))))

(comment
  (take 4 (get-input 3 integer-parse))
  (take 4 (get-input 2)))

(defn quantify [f xs]
  (count (filter f xs)))

(defn keep'
  "Like regular keep, but can take multiple colls"
  [f & colls]
  (filter some? (apply map f colls)))

(def cartesian-product combo/cartesian-product)

(defn test-pred [pred]
  (fn [x]
    (when (pred x) x)))

(defn self-loop [xs]
  (cartesian-product xs xs))