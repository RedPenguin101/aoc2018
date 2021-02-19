(ns aoc2018.day11)

(def input 9995)

(defn hundreds [num]
  (Long/parseLong (str (last (str (quot num 100))))))

(defn power-level [serial [x y]]
  (- (hundreds (* (+ x 10) (+ serial (* y (+ x 10))))) 5))

(comment
  (= 4 (power-level 8 [3 5]))
  (= -5 (power-level 57 [122 79]))
  (= 0 (power-level 39 [217 196]))
  (= 4 (power-level 71 [101 153])))

(comment
  [[0 0] [0 1] [0 2]
   [1 0] [1 1] [1 2]
   [2 0] [2 1] [2 2]]

  [[1 0] [1 1] [1 2]
   [2 0] [2 1] [2 2]
   [3 0] [3 1] [3 2]]

  '[[x y] [x+1 y] [x+2 y]
    []])


(comment
  (time (first (reverse
                (sort-by second (for [x (range 1 299)
                                      y (range 1 299)]
                                  [[x y]
                                   (reduce + (map (partial power-level 9995)
                                                  [[x y] [(+ 1 x) y] [(+ 2 x) y]
                                                   [x (+ 1 y)] [(+ 1 x) (+ 1 y)] [(+ 2 x) (+ 1 y)]
                                                   [x (+ 2 y)] [(+ 1 x) (+ 2 y)] [(+ 2 x) (+ 2 y)]]))])))))

  1)