(ns day18
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is are]]))

(comment
  "Day 18: Settlers of the North Pole

 Terms: Base, Lumber, Collection Area, ground/trees/lumberyard. adjacent/neigbourhood, 

 Acre change rules:
 * open -> trees when >=3 neigbours are trees (else open)
 * trees -> lumberyard when >=3 moore-neighbourhood are lumberyards (else trees)
 * lumberyard -> lumberyard when >=1 moore-neighbourhood are lumberyard AND >=1 moore-neighbourhood are trees. (else open)

 Concepts:
 HIGH: 
 * tick :: state -> state

 MED:
 * row-tick :: row, state -> row
 * next-acre :: content, content-of-neighbours -> new-content

 * content-of-neighbours :: state, coord -> #{contents}

 LOW:
 * moore-neigbourhood :: coord -> #{coord}

 Coordinate System: Seems very finite, so let's go with a simple matrix array where x is position in second layer, y is position in first layer 
 (so when doing get-in, remember to pass in [y x], not [x y])")

(defn value-at [state [x y]] (str (get-in state [y x]))) ;; nice string wrap to avoid dealing with character nastiness

(defn moore-neighbourhood [[x y]]
  (set (for [x' (range (dec x) (+ x 2)) y' (range (dec y) (+ y 2))
             :when (and (nat-int? x') (nat-int? y') (or (not= x x') (not= y y')))]
         [x' y'])))

(defn content-of-neighbours [state coord] (map #(value-at state %) (moore-neighbourhood coord)))

(defn next-acre [this-acre neighbour-acres]
  (let [{trees "|" lumbers "#" :or {trees 0 lumbers 0}} (frequencies neighbour-acres)]
    (case this-acre
      "." (if (>= trees 3) "|" ".")
      "|" (if (>= lumbers 3) "#" "|")
      "#" (if (and (>= lumbers 1) (>= trees 1)) "#" "."))))

(defn row-tick [state y]
  (->> (nth state y)
       (map-indexed (fn [x this-acre] (next-acre (str this-acre) (content-of-neighbours state [x y]))))
       (apply str)))

(defn tick [state] (mapv #(row-tick state %) (range (count state))))

;; solving the puzzle

(comment
  "Part 1 - easy"
  (let [{trees "|" lumbers "#"} (frequencies (mapcat #(map str %) (last (take 11 (iterate tick (str/split-lines (slurp "resources/day18input")))))))]
    (* trees lumbers))

  "Part 2 - check for some sort of repetition"
  (def thousand (doall (take 1000 (iterate tick (str/split-lines (slurp "resources/day18input")))))) ;; get 1000 iterations SUPER SLOW, like 25sec, but who cares 

  (->> thousand
       (drop 500)
       (map #(merge {\. 0, \# 0, \| 0} (frequencies (apply concat %))))
       (apply merge-with conj {\. [], \# [], \| []}))

  "slammed the above into Reveal, plotted a line graph on it. Sure enough, clear repetition starting at ~400. say 500 to be safe"

  (->> thousand
       (drop 500)
       (map #(get (frequencies (apply concat %)) \#))
       (partition 28))

  "Bit of fiddling with the partition value shows the periodicity is 28. So I'm looking for the 1,000,000,001th entry"

  "which is (a + (28 * b)). Need to solve for that"
  (map #(+ 500 (* 28 %)) (range 35714260 35714284))
  (+ 497 (* 28 35714268))
  ;; => 1000000001

  "There is is, the entry at 497 should be the same as the one at 1,000,000,001"

  (= (nth thousand 497)
     (nth thousand (+ 28 497))) ;; quick check

  (frequencies (apply concat (nth thousand 497)))
  ;; => {\. 1571, \# 311, \| 618}
  (* 311 618)
  ;; => 192198

  "This is wrong apparently! Probably off by one, try starting at 496 instead"
  (frequencies (apply concat (nth thousand 496)))
  ;; => {\. 1570, \# 322, \| 608}
  (* 322 608)
  ;; => 195776
  "Success!")

;; examples and tests

(def example-init  ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.")
(def example-1-min ".......##.\n......|###\n.|..|...#.\n..|#||...#\n..##||.|#|\n...#||||..\n||...|||..\n|||||.||.|\n||||||||||\n....||..|.")
(def example-2-min ".......#..\n......|#..\n.|.|||....\n..##|||..#\n..###|||#|\n...#|||||.\n|||||||||.\n||||||||||\n||||||||||\n.|||||||||")
(def example-3-min ".......#..\n....|||#..\n.|.||||...\n..###|||.#\n...##|||#|\n.||##|||||\n||||||||||\n||||||||||\n||||||||||\n||||||||||")
(def example-4-min ".....|.#..\n...||||#..\n.|.#||||..\n..###||||#\n...###||#|\n|||##|||||\n||||||||||\n||||||||||\n||||||||||\n||||||||||")
(def example-5-min "....|||#..\n...||||#..\n.|.##||||.\n..####|||#\n.|.###||#|\n|||###||||\n||||||||||\n||||||||||\n||||||||||\n||||||||||")
(def example-6-min "...||||#..\n...||||#..\n.|.###|||.\n..#.##|||#\n|||#.##|#|\n|||###||||\n||||#|||||\n||||||||||\n||||||||||\n||||||||||")
(def example-7-min "...||||#..\n..||#|##..\n.|.####||.\n||#..##||#\n||##.##|#|\n|||####|||\n|||###||||\n||||||||||\n||||||||||\n||||||||||")
(def example-8-min "..||||##..\n..|#####..\n|||#####|.\n||#...##|#\n||##..###|\n||##.###||\n|||####|||\n||||#|||||\n||||||||||\n||||||||||")
(def example-9-min "..||###...\n.||#####..\n||##...##.\n||#....###\n|##....##|\n||##..###|\n||######||\n|||###||||\n||||||||||\n||||||||||")
(def example-10-min ".||##.....\n||###.....\n||##......\n|##.....##\n|##.....##\n|##....##|\n||##.####|\n||#####|||\n||||#|||||\n||||||||||")

(deftest neighbour-coord-test
  (are [coord results] (= (moore-neighbourhood coord) results)
    [0 0] #{[1 0] [1 1] [0 1]}
    [1 0] #{[0 0] [1 1] [2 0] [2 1] [0 1]}
    [1 1] #{[2 2] [0 0] [1 0] [0 2] [2 0] [2 1] [1 2] [0 1]}))

(deftest value-at-test
  (let [state (str/split-lines ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n ...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.")]
    (are [input output] (= (value-at state input) output)
      [0 0] "."
      [1 0] "#"
      [7 0] "|"
      [7 1] "#")))

(deftest content-of-neighbours-test
  (let [state (str/split-lines ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n ...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|.")]
    (are [input output] (= (content-of-neighbours state input) output)
      [0 0] '("#" "." ".")
      [1 0] '("." "." "." "." ".")
      [7 0] '("#" "#" "#" "|" ".")
      [7 1] '("." "#" "#" "#" "|" "|" "." "."))))

(deftest tick-test
  (is (= (str/split-lines example-1-min) (last (take 2 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-2-min) (last (take 2 (iterate tick (str/split-lines example-1-min))))))
  (is (= (str/split-lines example-2-min) (last (take 3 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-3-min) (last (take 4 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-4-min) (last (take 5 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-5-min) (last (take 6 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-6-min) (last (take 7 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-7-min) (last (take 8 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-8-min) (last (take 9 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-9-min) (last (take 10 (iterate tick (str/split-lines example-init))))))
  (is (= (str/split-lines example-10-min) (last (take 11 (iterate tick (str/split-lines example-init)))))))

