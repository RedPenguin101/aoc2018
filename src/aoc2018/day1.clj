(ns aoc2018.day1
  (:require [clojure.string :as str]))

(def input (map #(Long/parseLong %)
                (str/split-lines (slurp "resources/day1input"))))

(reduce + (concat input input input))

(reduce conj '() [1 -1])

(reduce (fn [seen new]
          (let [new-acc (+ new (last seen))]
            (if ((set seen) new-acc)
              (reduced new-acc)
              (conj seen new-acc))))
        '(0)
        [3 3 4 -2 -4])

(defn find-repeat [xs]
  (reduce (fn [{:keys [seen acc] :as A} new]
            (let [new-acc (+ new acc)]
              (if (seen new-acc)
                (reduced new-acc)
                (-> A (update :seen conj new-acc)
                    (assoc :acc new-acc)))))
          {:seen #{0}
           :acc 0}
          (cycle xs)))

(comment
  (find-repeat [1 -1])
  (find-repeat [3 3 4 -2 -4])
  (find-repeat [-6 3 8 5 -6])
  (find-repeat [7 7 -2 -7 -4])
  (time (find-repeat input))
  ;; => 82516
  )