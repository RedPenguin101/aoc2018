(ns aoc2018.day2
  (:require [clojure.string :as str]))

(defn contains-letter-n-times [n]
  (fn [id]
    ((set (vals (frequencies id))) n)))

((contains-letter-n-times 2) "abcdef")
((contains-letter-n-times 2) "bababc")
((contains-letter-n-times 3) "bababc")

(def input (str/split-lines (slurp "resources/day2input")))

(* (count (filter (contains-letter-n-times 2) input))
   (count (filter (contains-letter-n-times 3) input)))

(defn letters-in-common [s1 s2]
  (remove nil? (map #(when (= %1 %2) %1) s1 s2)))

(letters-in-common "abcde" "axcye")
(letters-in-common "fghij" "fguij")

(def example ["abcde"
              "fghij"
              "klmno"
              "pqrst"
              "fguij"
              "axcye"
              "wvxyz"])

(apply str (first (for [s1 input
                        s2 input
                        :let [lic (letters-in-common s1 s2)]
                        :when (= (dec (count s1)) (count lic))]
                    lic)))
;; => "ighfbyijnoumxjlxevacpwqtr"
