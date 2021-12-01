(ns aoc2018.day17)

"vertical slice
 scan
 sand, clay
 vein
 `x=495, y=2..7` a vein of clay at x=495, running from y=2 to y=7 
 `y=7, x=495..501` a seam of clay at x=495, running from y=2 to y=7 
 x=distance to right
 y=distance down
 spring [500,0]
 produces squares of water
 water flows in path through sand, blocked by clay
 water flows down if possible, left and right otherwise
 water settles on clay or on top of other settled water
 
 How many tiles can the water reach within the range of y values in your scan?
 
 CONCEPTS
 Slice (grid)
 Square [X Y]
 Clay / Sand
 "
"
   44444455555555
   99999900000000
   45678901234567
 0 ......+.......
 1 ......0.....#.
 2 .#..#0000...#.
 3 .#..#00#0.....
 4 .#..#00#0.....
 5 .#00000#0.....
 6 .#00000#0.....
 7 .#######0.....
 8 ........0.....
 9 ...000000000..
10 ...0#00000#0..
11 ...0#00000#0..
12 ...0#00000#0..
13 ...0#######0..
 "

"Something like a tree: 
 * node-a: if no down, try down (success: node-b)
 * if clay, and if no left, try left from node-a (success: node-c, retry)
 * if clay, and if no right, try right from node-a (success: node-d, retry)
 * if clay, exhausted backtrack to parent of node-a and repeat"

(def clay #{[495 2] [495 3] [495 4] [495 5] [495 6] [495 7]
            [496 7] [497 7] [498 7] [499 7] [500 7] [501 7]
            [501 3] [501 4] [501 5] [501 6]
            [498 2] [498 3] [498 4]
            [506 1] [506 2]
            [498 10] [498 11] [498 12] [498 13]
            [504 10] [504 11] [504 12] [504 13]
            [499 13] [500 13] [501 13] [502 13] [503 13]})
(apply max (map second clay))

(defn successor [[x y] dir]
  (case dir
    :down [x (inc y)]
    :left [(dec x) y]
    :right [(inc x) y]))

{:left [1 2]
 :down :clay
 :right :clay}

(defn next-dir [node]
  (case (:tried node)
    #{} :down
    #{:right} :down
    #{:left} :down
    #{:down} :left
    #{:down :left} :right
    #{:down :right} :left
    #{:down :left :right} nil))

(defn exhausted? [node]
  (if (next-dir node) false true))

(def ymax 13)

(defn expand [tree this it]
  (let [this-node (tree this)
        dir (next-dir this-node)]
    #_(println [this this-node dir it])
    (cond
      (> it 1000) [:break tree]
      (and (= :root (:parent this-node)) (nil? dir)) tree
      (nil? dir) (recur tree (:parent this-node) (inc it))

      (>= (second (successor this dir)) ymax) (recur tree (:parent this-node) (inc it))

      (clay (successor this dir))
      (recur (update-in tree [this :tried] conj dir) this (inc it))

      :else (recur (-> tree
                       (update-in [this :tried] conj dir)
                       (assoc-in [(successor this dir) :parent] this)
                       (assoc-in [(successor this dir) :tried] (case dir :right #{:left} :left #{:right} :down #{})))
                   (successor this dir)
                   (inc it)))))

"Missing piece: you can go left or if what is below you is water or clay"

(expand {[500 0] {:parent :root
                  :tried #{}}} [500 0] 0)