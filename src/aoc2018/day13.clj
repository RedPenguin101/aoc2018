(ns aoc2018.day13
  (:require [clojure.string :as str]
            [aoc2018.aoc-utils :refer [get-input]]))

(def init-dir {\< :left
               \> :right
               \^ :up
               \v :down})


(def turns {\\ {:up :left
                :right :down
                :down :right
                :left :up}
            \/  {:up :right
                 :right :up
                 :down :left
                 :left :down}})

(def intersection-turns {:turn-left {:up :left
                                     :down :right
                                     :left :down
                                     :right :up}
                         :go-straight {:up :up
                                       :down :down
                                       :left :left
                                       :right :right}
                         :turn-right {:up :right
                                      :down :left
                                      :left :up
                                      :right :down}})

(def ex (slurp "resources/day13example"))

(defn parse-row [y row]
  (vec (keep-indexed (fn [x sym] (when (not (#{\- \| \space} sym)) [[x y] sym])) row)))

(defn parse [rows]
  (apply concat (vec (map-indexed #(parse-row %1 %2) rows))))

(defn track-and-carts [A [coord sym]]
  (if (#{\\ \/ \+} sym)
    (update A :track conj [coord sym])
    (update A :carts conj {:pos coord
                           :dir (init-dir sym)
                           :next-intersection :turn-left})))

(comment ;;parsings
  (def exp (parse (str/split-lines ex)))

  (let [{:keys [track carts]} (reduce track-and-carts {} exp)]
    (def track (into {} track))
    (def carts carts))


  (let [{:keys [track carts]} (reduce track-and-carts {} (parse (get-input 13)))]
    (def track (into {} track))
    (def carts carts))

  1)

(defn sort-carts [carts]
  (sort-by (juxt #(second (:pos %)) #(first (:pos %))) carts))

(defn turn [{:keys [dir next-intersection] :as cart} track-piece]
  (assoc cart :dir
         (cond (nil? track-piece) dir
               (#{\+} track-piece) (get-in intersection-turns [next-intersection dir])
               :else (get-in turns [track-piece dir]))))

(defn intersection [next-intersection track-piece]
  (if (#{\+} track-piece) (get {:turn-left :go-straight
                                :go-straight :turn-right
                                :turn-right :turn-left}
                               next-intersection)
      next-intersection))


(defn next-pos [{:keys [pos dir] :as cart}]
  #_(tap> cart)
  (assoc cart :pos (let [[x y] pos]
                     (case dir
                       :up [x (dec y)]
                       :right [(inc x) y]
                       :down [x (inc y)]
                       :left [(dec x) y]))))

(defn move [track cart]
  #_(tap> (:pos cart))
  (let [track-piece (track (:pos cart))]
    (-> cart
        (turn track-piece)
        (update :next-intersection intersection track-piece)
        next-pos)))

(defn collision? [carts]
  (let [freqs (frequencies (map :pos carts))]
    (when (some #{2} (vals freqs))
      freqs)))

(defn tick [track moved unmoved]
  (cond (empty? unmoved) moved
        (collision? (concat moved unmoved)) {:collision (collision? (concat moved unmoved))}
        :else (recur track
                     (conj moved (move track (first unmoved)))
                     (rest unmoved))))

(defn run [track carts it]
  (let [new-carts (tick track [] (sort-carts carts))]
    (cond (> it 1000) :break
          (map? new-carts) new-carts
          :else (recur track new-carts (inc it)))))

(comment
  (run track carts 0)

  (sort-carts carts)

  (get track [36 86]))

(comment
  carts
  (tick track [] (sort-carts '({:pos [9 3]
                                :dir :down
                                :next-intersection :turn-left}
                               {:pos [2 0]
                                :dir :right
                                :next-intersection :turn-left})))

  (tick track [] (sort-carts [{:pos [3 0]
                               :dir :right
                               :next-intersection :turn-left}
                              {:pos [9 4]
                               :dir :down
                               :next-intersection :turn-left}]))

  (tick track [] (sort-carts [{:pos [4 0]
                               :dir :right
                               :next-intersection :turn-left}
                              {:pos [10 4]
                               :dir :right
                               :next-intersection :go-straight}]))
  1)
