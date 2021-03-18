(ns day15
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]))

;; helpers and debug

(defn tap>> [note x]
  (tap> [note x])
  x)

(defn grid->string
  "Takes a map of [x y]->val, and a function which transforms values to strings, and returns
   a string representation of the grid, with 0,0 being the top left"
  [f m]
  (let [[xs ys] (apply map vector (keys m))]
    (str/join
     "\n"
     (map str/join
          (partition (- (inc (apply max xs)) (apply min xs))
                     (for [y (range (apply min ys) (inc (apply max ys)))
                           x (range (apply min xs) (inc (apply max xs)))]
                       (apply str (f (m [x y])))))))))

(defn print-grid [m]
  (println (grid->string
            (fn [[typ _]]
              (if (nil? typ) "#" (typ {:elf "E" :goblin "G" :empty "."})))
            m))
  (doall (map println (filter (fn [[k [typ hp]]] (#{:goblin :elf} typ)) m))))


;; parsing

(defn parse-input [string]
  (into {} (apply concat
                  (map-indexed
                   (fn parse-row [y row]
                     (remove
                      nil?
                      (map-indexed
                       (fn [x c] (case c
                                   \. [[x y] [:empty]]
                                   \# nil
                                   \G [[x y] [:goblin 200]]
                                   \E [[x y] [:elf 200]])) row)))
                   (str/split-lines string)))))


(def example (parse-input "#######
#.G.E.#
#E.G.E#
#.G.E.#
#######
"))

(def example2 (parse-input "#######
#E..G.#
#...#.#
#.G.#G#
#######
"))

(def example3 (parse-input "#######
#.E...#
#.....#
#...G.#
#######
"))

(def example4-0 (parse-input "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########
"))

(def example4-1 (parse-input "#########
#.G...G.#
#...G...#
#...E..G#
#.G.....#
#.......#
#G..G..G#
#.......#
#########"))

(def example4-2 (assoc (parse-input "#########
#..G.G..#
#...G...#
#.G.E.G.#
#.......#
#G..G..G#
#.......#
#.......#
#########")
                       [4 2] [:goblin 197]
                       [4 3] [:elf 197]))

(def example4-3 (parse-input "#########
#.......#
#..GGG..#
#..GEG..#
#G..G...#
#......G#
#.......#
#.......#
#########
"))

(def example5-0 (parse-input
                 "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
"))


(def example5-1 (assoc (parse-input
                        "#######
#..G..#
#...EG#
#.#G#G#
#...#E#
#.....#
#######
")
                       [4 2] [:elf 197]
                       [5 2] [:goblin 197]
                       [5 3] [:goblin 197]
                       [5 4] [:elf 197]))



(def example5-2 (assoc (parse-input
                        "#######
#...G.#
#..GEG#
#.#.#G#
#...#E#
#.....#
#######
")
                       [4 2] [:elf 188]
                       [5 2] [:goblin 194]
                       [5 3] [:goblin 194]
                       [5 4] [:elf 194]))

(def input (parse-input (slurp "resources/day15input")))

"
 #: wall
 .: open
 Units:
   G: Goblin
   E: elf

 ROUND each unit takes a TURN (Read order: l->r, top->bottom)
 TURN tries to MOVE (up, down, left, right) into RANGE of ENEMY and ATTACK
 RANGE: adjacent to UNIT (up, down, left right)
 MOVE: units move 1 square on shortest path to the CLOSEST square which is in range of enemy
 CLOSEST (manhattan dist) open square in RANGE that has OPEN PATH (open square, no units) 
 on tie, choose highest reading order
 ATTACK: from all enemies in range, choose the one with the fewest hitpoints 
 (tie break on reading order position)
 Units have 200 hitpoints, and each attack takes off 3 hitpoints.

 LEVEL 1:
 run :: initial-state -> final state (first turn when a unit finds no targets)
 round :: state -> state (all units have taken turn)
 turn :: state, coord -> state (unit at coord moves to new position and possibly attacks)

 LEVEL 2: (implement turn: unit at coord picks next position, attacks if possible)
 move :: state, coord -> state (with unit at coord now in new coord)
 attack :: state, unit-coord -> state (with target having -3hp, or unchanged if no valid target)

 LEVEL 3:
 next-postion :: state, coord -> coord that the unit at input coord will move to
 move-unit :: state, from-coord, to-coord -> state (with unit that was at from now at to)

 pick-target :: state, unit-coord -> coord of target (or nil if none)
 hit-unit :: state, coord -> state (replacing unit at coord with [unit-type (- hitpoints 3)])

 LEVEL 3:
 closest :: state, coord, candidates -> candidate which input coord is closest to
 attack-squares :: state, coord -> coords in range of enemy that are accessible to unit
 enemy units :: state, coord -> [coord], where coords are enemies of the unit at coord

 LEVEL 4:
 turn-order :: state -> [coord] in read-order
 shortest-path :: open-coords, from-coord, to-coord -> [coord], shortest path from a to b
 open-coords :: state -> [coord] returns all coordinates that are open

 STATE REPRESENTATION:
 unit: [type hitpoints]
 state: coord -> unit | [:empty]
 "

(defn- read-order [[x y]] [y x])

;; Level 1: Coordinates

(defn- neigbours
  "Returns the squares directly up, down, left and right of the input sqaure"
  [[x y]]
  #{[(inc x) y] [(dec x) y]
    [x (inc y)] [x (dec y)]})

(defn- open-coords
  "Returns all currently empty squares in the state"
  [state]
  (keep (fn [[coord [occupant]]] (when (= :empty occupant) coord)) state))

(defn- open-adjacent-squares
  "Given a set of candidate squares, returns any of the candidates which are adjacent to the
   input square"
  [candidates this]
  (sort-by read-order (set/intersection (set candidates) (neigbours this))))

(defn- shortest-path
  "Given a seq of open sqaures, returns the shortest path through those squares from 'from' to 'to'.
   Returns nil if there is no path"
  ([open from to] (shortest-path open [[from]] #{} to))
  ([open frontier explored goal]
   (let [path (first frontier)
         this (last path)]
     #_(tap> frontier)
     (cond (empty? frontier) nil
           (= this goal) path
           (explored this) (recur open (rest frontier) explored goal)
           :else
           (recur open
                  (concat (rest frontier) (map #(conj path %) (open-adjacent-squares open this)))
                  (conj explored this)
                  goal)))))

;; level 2: State and paths

(def enemy-of {:goblin :elf :elf :goblin})

(defn spaces-containing-full
  "Returns the spaces containing the elements of the set 'contents'"
  [state contents]
  (sort-by read-order (keep (fn [[coord [type hp]]] (when (contents type) [coord [type hp]])) state)))

(def spaces-containing (comp #(sort-by read-order %) #(map first %) spaces-containing-full))

(defn- turn-order
  "Given the state, returns a sequence of coordinates with units in them, in the order in which they
   will take their turn"
  [state]
  (spaces-containing state #{:goblin :elf}))

(defn- attack-squares
  "Returns all the spaces where the unit at input coord will be in range of an enemy unit
   (there may or may not be a valid path to the square)"
  [state coord]
  (mapcat #(open-adjacent-squares
            (spaces-containing state #{:empty}) %)
          (spaces-containing state #{(enemy-of (first (state coord)))})))

(defn- path-to-closest-attack-square
  [state coord]
  (if (or (nil? (state coord)) (= [:empty] (state coord)))
    (throw (ex-info "Can't be called on an empty square" {:state state :coord coord}))
    (->> (attack-squares state coord)
         (sort-by read-order)
         (keep #(shortest-path (spaces-containing state #{:empty}) coord %))
         (sort-by count)
         first)))

;; level 3 moving

(defn move [state current-coord]
  (if-let [[_ move-to] (path-to-closest-attack-square state current-coord)]
    (-> state
        (assoc move-to (state current-coord))
        (assoc current-coord [:empty]))
    state))

(defn pick-target [state coord]
  (->> (spaces-containing-full state #{(enemy-of (first (state coord)))})
       (filter #((neigbours coord) (first %)))
       (sort-by (fn [[coord [_ hp]]] [hp (read-order coord)]))
       (ffirst)))

(defn attack [state target]
  (if target
    (let [[_ hp] (state target)]
      (if (> hp 3)
        (update-in state [target 1] - 3)
        (assoc state target [:empty])))
    state))

(defn move-and-attack [state current-coord]
  (if-let [[_ move-to] (path-to-closest-attack-square state current-coord)]
    (let [new-state (-> state
                        (assoc move-to (state current-coord))
                        (assoc current-coord [:empty]))]
      (attack new-state (pick-target new-state move-to)))
    state))


(comment
  (attack {[1 1] [:goblin 3]} [1 1]))

(defn turn [state coord]
  (if (= [:empty] (state coord))
    state
    (if-let [target (pick-target state coord)]
      (attack state target)
      (move-and-attack state coord))))

(defn round [state]
  (reduce turn state (turn-order state)))

(comment
  (= example5-1 (round example5-0)))

(defn play [state it]
  (cond (> it 1000) :break
        (or (empty? (spaces-containing state #{:goblin}))
            (empty? (spaces-containing state #{:elf})))
        [it (into {} (spaces-containing-full state #{:goblin :elf})) (* (dec it) (apply + (map (comp second second) (spaces-containing-full state #{:goblin :elf}))))]
        :else (recur (round state) (inc it))))

(comment
  (is (= [47 {[5 3] [:goblin 59], [2 2] [:goblin 131], [1 1] [:goblin 200], [5 5] [:goblin 200]} 27730]
         (play example5-0 0)))

  ((last (take 28 (iterate round example5-0))) [5 4])
  ((last (take 29 (iterate round example5-0))) [5 4])

  (= example4-1 (round example4-0))
  (= (assoc example4-2
            [4 2] [:goblin 197]
            [4 3] [:elf 197]) (round example4-1))
  (print-grid example4-2)
  (print-grid (-> example4-2 round))


  (= example5-2 (round example5-1))
  (into {} (spaces-containing-full (last (take 49 (iterate round example5-0))) #{:goblin :elf}))

  (print-grid (last (take 49 (iterate round example5-0)))))

(comment
  (is (= example4-1 (reduce turn example4-0 (turn-order example4-0))))
  (merge-with vector example4-2 (reduce turn example4-1 (turn-order example4-1)))
  (print-grid (reduce turn example4-1 (turn-order example4-1)))
  (print-grid example4-2)
  (print-grid (reduce turn example4-2 (turn-order example4-2)))


  (let [state (reduce turn example4-2 [[3 1] [5 1] [4 2] [2 3] [4 3] [6 3] [1 5] [4 5]])]
    (turn state [7 5]))
  '([3 1] [5 1] [4 2] [2 3] [4 3] [6 3] [1 5] [4 5] [7 5]))

(comment
  (= example4-2 (reduce move example4-1 (turn-order example4-1)))
  (sort-by (comp read-order first) (merge-with vector example4-2 (reduce move example4-1 (turn-order example4-1))))
  (sort-by (comp read-order first) example4-1))

(defn test-map
  [state assertions]
  (for [[coord unit] assertions]
    (state coord)))

(comment
  (test-map example [[[1 2] [:goblin 200]]])

  example

  (turn {[4 3] [:empty]
         [2 2] [:elf 200]
         [0 0] [:goblin 9]
         [1 0] [:empty]
         [2 3] [:goblin 2]
         [3 3] [:empty]
         [1 1] [:empty]
         [3 4] [:goblin 1]
         [4 2] [:empty]
         [3 0] [:empty]
         [4 1] [:empty]
         [1 4] [:empty]
         [1 3] [:empty]
         [0 3] [:empty]
         [2 4] [:empty]
         [0 2] [:empty]
         [2 0] [:empty]
         [0 4] [:empty]
         [3 1] [:empty]
         [2 1] [:goblin 4]
         [4 4] [:empty]
         [1 2] [:empty]
         [3 2] [:goblin 2]
         [0 1] [:empty]
         [4 0] [:empty]}
        [2 2]))

(let [s (parse-input "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######")]
  (play s 0))

(let [s (parse-input "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######")]
  (play s 0))

(comment
  (play example5-0 0))

(deftest play-test
  (is (= [47 {[5 3] [:goblin 59], [2 2] [:goblin 131], [1 1] [:goblin 200], [5 5] [:goblin 200]} 27730]
         (play example5-0 0))))

(deftest target-picking
  (let [x (parse-input
           "G....\n..G..\n..EG.\n..G..\n...G.")]
    (is (= [3 2] (pick-target (assoc x
                                     [0 0] [:goblin 9]
                                     [2 1] [:goblin 4]
                                     [3 2] [:goblin 2]
                                     [2 3] [:goblin 2]
                                     [3 4] [:goblin 1])


                              [2 2])))))

(deftest pathing
  (is (= [[7 1] [7 0] [6 0] [5 0]]
         (let [state (parse-input "#...E...#\n#.G....G#")]
           (path-to-closest-attack-square state [7 1]))))

  (is (= [[1 1] [2 1] [3 1]]
         (path-to-closest-attack-square example2 [1 1])))

  (is (= [[4 1] [3 1] [2 1]]
         (path-to-closest-attack-square example2 [4 1])))

  (is (= [[2 3] [2 2] [2 1]]
         (path-to-closest-attack-square example2 [2 3])))

  (is nil? (path-to-closest-attack-square example2 [5 3]))
  (is (thrown? Exception (path-to-closest-attack-square example2 [2 2])))
  (is (thrown? Exception (path-to-closest-attack-square example2 [4 0])))

  (is (= [[2 1] [3 1] [4 1] [4 2]]
         (path-to-closest-attack-square example3 [2 1])))
  (is (= example4-1 (reduce move example4-0 (turn-order example4-0)))))

(deftest shortest
  (is (= [[2 1] [2 2]] (shortest-path (spaces-containing example #{:empty}) [2 1] [2 2])))
  (is (nil? (shortest-path example [2 1] [5 1])))
  (is (= [[1 1] [2 1] [3 1] [3 2]]
         (shortest-path (spaces-containing example2 #{:empty}) [1 1] [3 2]))))

(deftest low-level
  (is (= '([1 1] [2 1] [3 1] [1 2])
         (sort-by read-order [[1 1] [2 1] [1 2] [3 1]])))
  (is (= '([2 2] [3 3] [1 1] [4 2] [5 3] [1 3] [5 1] [3 1])
         (open-coords example)))
  (is (= [[2 1] [4 1] [1 2] [3 2] [5 2] [2 3] [4 3]]
         (turn-order example))))
