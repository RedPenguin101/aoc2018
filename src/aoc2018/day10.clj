(ns aoc2018.day10
  (:require [clojure.string :as str]))

(defn parse-point [line]
  (mapv (comp #(Long/parseLong %) str/trim) (re-seq #"[ -]?\d+" line)))

(def points (mapv parse-point (str/split-lines (slurp "resources/day10example"))))

(defn move
  ([[x-pos y-pos x-vel y-vel]]
   [(+ x-pos x-vel) (+ y-pos y-vel) x-vel y-vel])
  ([times [x-pos y-pos x-vel y-vel]]
   [(+ x-pos (* times x-vel)) (+ y-pos (* times y-vel)) x-vel y-vel]))

(defn grid-boundaries [points]
  {:x-max (apply max (map first points))
   :x-min (apply min (map first points))
   :y-max (apply max (map second points))
   :y-min (apply min (map second points))})

(defn points-grid [points]
  (let [b (grid-boundaries points)
        p (set (map (partial take 2) points))]
    (->> (for [y (range (:y-min b) (inc (:y-max b)))
               x (range (:x-min b) (inc (:x-max b)))]
           (if (contains? p [x y]) \# \.))
         (partition (inc (- (:x-max b) (:x-min b))))
         (map str/join))))

(def points2 (mapv parse-point (str/split-lines (slurp "resources/day10input"))))

(defn find-min [points]
  (reduce (fn [A new]
            (let [old-dims (grid-boundaries (:points A))
                  new-dims (grid-boundaries new)]
              (if (> (- (:y-max old-dims) (:y-min old-dims))
                     (- (:y-max new-dims) (:y-min new-dims)))
                (-> A
                    (assoc :points new)
                    (update :seconds inc))
                (reduced A))))
          {:points points
           :seconds 0}
          (iterate (partial map move) (map move points))))

(comment
  (time (points-grid (:points (find-min points2))))

  (:seconds (find-min points2)))
