(ns aoc2018.day6
  (:require [clojure.string :as str]
            [aoc2018.kdtree :as kdt]))

(def example "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9
")

(defn input-parse [string]
  (map (fn [line] (mapv #(Long/parseLong %) (str/split line #", "))) (str/split-lines string)))

(def example-input (input-parse example))

(kdt/kd-tree example-input 0)

(defn grid-size [input-coords]
  {:xmax (apply max (map first input-coords))
   :xmin (apply min (map first input-coords))
   :ymax (apply max (map second input-coords))
   :ymin (apply min (map second input-coords))})

(defn grid [input-coords]
  (let [{:keys [xmax ymax xmin ymin]} (grid-size input-coords)]
    (for [x (range xmin (inc xmax))
          y (range ymin (inc ymax))]
      [x y])))

(defn- md [[^Integer x1 ^Integer y1] [^Integer x2 ^Integer y2]]
  (+ (Math/abs (unchecked-subtract-int x1 x2))
     (Math/abs (unchecked-subtract-int y1 y2))))

(defn finite? [{:keys [xmax ymax xmin ymin]} [_ points]]
  (not-any? (fn [[x y]] (or (= x xmax) (= x xmin)
                            (= y ymax) (= y ymin)))
            points))

(def input (input-parse (slurp "resources/day6input")))

(defn largest-finite-zone [inputs]
  (let [tree (kdt/kd-tree inputs 0)]
    (->> inputs
         grid
         (map #(vector % (kdt/nn-search tree % md)))
         (reduce (fn [A [point input]]
                   (if (= :draw (first input))
                     A
                     (update A input conj point)))
                 {})
         (filter (partial finite? (grid-size inputs)))
         (sort-by (comp count second))
         last
         second
         count)))

(comment
  (largest-finite-zone example-input)
  (largest-finite-zone input)

  (double (/ 5000000 35))

  "36 seconds - too slow!")
