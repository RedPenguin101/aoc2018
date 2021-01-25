(ns aoc2018.day5
  (:require [clojure.string :as str]))

(defn eliminates? [a b]
  (and (not= a b)
       (= (str/upper-case a)
          (str/upper-case b))))

(comment
  (eliminates? "a" "a")
  (eliminates? "a" "B")
  (eliminates? "a" "A")
  (eliminates? "A" "a")
  (eliminates? "a" "A")

  "Also works on chars"
  (eliminates? \a \A)
  (eliminates? \a \b))

(defn react [polymer]
  (reverse (reduce (fn [A unit]
                     (if ((fnil eliminates? \!) (first A) unit)
                       (rest A)
                       (cons unit A)))
                   '()
                   polymer)))

(defn fully-react [polymer]
  (reduce (fn [last new] (if (= last new) (reduced new) new))
          (iterate react polymer)))

(count (fully-react (-> "resources/day5input" slurp str/trim-newline)))

(time
 (sort-by second
          (for [ltr "abcdefghijklmnopqrstuvwxyz"]
            [(str ltr) (count (fully-react (str/escape (-> "resources/day5input" slurp str/trim-newline)
                                                       {ltr "" (first (str/upper-case ltr)) ""})))])))
