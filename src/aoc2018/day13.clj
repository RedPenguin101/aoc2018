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
  (def exp (parse (str/split-lines (slurp "resources/day13example"))))

  (let [{:keys [track carts]} (reduce track-and-carts {} exp)]
    (def track-ex (into {} track))
    (def carts-ex carts))


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
    (when (some (#{2}) (vals freqs))
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

"There isn't much you can do to prevent crashes in this ridiculous system. However, by 
 predicting the crashes, the Elves know where to be in advance and instantly remove the two 
 crashing carts the moment any crash occurs.
 
 figure out where the last cart that hasn't crashed will end up."

"Run tick, but instead of returning the collision, remove the carts that have collided"

(defn remove-collisions [carts]
  (vals (reduce (fn [A cart]
                  (let [cart-pos (:pos cart)]
                    (if (A cart-pos)
                      (dissoc A cart-pos A)
                      (assoc A cart-pos cart))))
                {}
                carts)))

(defn tick2 [track moved unmoved]
  (cond (empty? unmoved) moved
        :else (recur track
                     (remove-collisions (conj moved (move track (first unmoved))))
                     (rest unmoved))))

(defn run2 [track carts it]
  #_(when (zero? (mod it 1000))
      (tap> [it (count carts) carts]))
  (let [new-carts (tick2 track [] (sort-carts carts))]
    (cond (> it 100000) :break
          (= it 35764) [it new-carts]
          (<= (count new-carts) 1) [it new-carts]
          :else (recur track new-carts (inc it)))))

(comment
  (let [exp (parse (str/split-lines (slurp "resources/day13example2")))
        {:keys [track carts]} (reduce track-and-carts {} exp)]
    (def track-ex2 (into {} track))
    (def carts-ex2 carts))

  (run2 track-ex2 carts-ex2 0)
  (run2 track carts 0)
  1)