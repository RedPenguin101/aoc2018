(ns aoc2018.day12
  (:require [clojure.string :as str]
            [aoc2018.aoc-utils :refer [get-input]]))

(defn ->initial-state [string]
  (map-indexed #(vector %1 (str %2)) (subs string 15)))

(defn ->rule [rul]
  (str/split rul #" => "))

(def ex-pots (->initial-state "initial state: #..#.#..##......###...###"))
(def ex-rules (into {} (map ->rule (str/split-lines "...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"))))

(defn test-rules [rules five-pots]
  [(first (nth five-pots 2)) (rules (apply str (map second five-pots)) ".")])

(defn pad [pots]
  (let [start (ffirst pots) end (first (last pots))]
    (concat (map #(vector % ".") (range (- start 5) start))
            pots
            (map #(vector % ".") (range (inc end) (+ end 6))))))

(defn trim [pots]
  (->> pots
       (drop-while #(= "." (second %)))
       reverse
       (drop-while #(= "." (second %)))
       reverse))

(defn step [rules pots]
  (->> (pad pots)
       (partition 5 1)
       (map #(test-rules rules %))
       trim))

(defn score [pots]
  (reduce + (map first (filter #(= "#" (second %)) pots))))

(let [[init-st _ & rules] (get-input 12)]
  (def pots (->initial-state init-st))
  (def rules (into {} (map ->rule rules))))

(comment
  (time (score (last (take 10001 (iterate (partial step rules) pots)))))

  (map score (take 300 (iterate (partial step rules) pots)))
  (score (last (take 150 (iterate (partial step rules) pots))))
  ;; => 12653
  (score (last (take 151 (iterate (partial step rules) pots))))
  ;; => 12726
  (- 12726 12653)
  (- (score (last (take 156 (iterate (partial step rules) pots))))
     (score (last (take 157 (iterate (partial step rules) pots)))))

  (+ (* 73 (- 157 150)) 12653)
  (+ (* 73 (- 50000000001 150)) 12653)
  ;; => 3650000001776


  1)
