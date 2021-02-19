(ns aoc2018.day3norvig
  (:require [aoc2018.aoc-utils :refer [get-input integer-parse
                                       cartesian-product
                                       quantify]]))

(def input (get-input 3 integer-parse))


(defn coords [[_id x y w h]]
  (cartesian-product (range x (+ x w)) (range y (+ y h))))

(defn times-claimed [claims]
  (->> claims
       (mapcat coords)
       frequencies))

(let [claimed (times-claimed input)]
  (->> claimed
       keys
       (quantify #(>= (claimed %) 2))))
;; => 97218


(defn no-overlap? [C]
  (fn [claim]
    (when (every? #{1} (map C (coords claim))) claim)))

(let [C (times-claimed input)]
  (->> input (some (no-overlap? C)) first time))
;; => 717
