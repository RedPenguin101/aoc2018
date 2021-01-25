(ns aoc2018.kdtree)

(defn- undraw [x]
  (if (= :draw (first x))
    (recur (second x))
    x))

(defn- remove-nils [m]
  (reduce-kv #(if (nil? %3) %1
                  (assoc %1 %2 %3)) {} m))

(defn kd-tree [points depth]
  (if (empty? points)
    nil
    (let [dim (mod depth 2)
          split-point (quot (count points) 2)
          sorted-points (vec (sort-by #(nth % dim) points))]
      (remove-nils {:left (kd-tree (subvec sorted-points 0 split-point) (inc depth))
                    :right (kd-tree (subvec sorted-points (inc split-point)) (inc depth))
                    :dimension dim
                    :value (nth sorted-points split-point)}))))

(defn- md [[^Integer x1 ^Integer y1] [^Integer x2 ^Integer y2]]
  (+ (Math/abs (unchecked-subtract-int x1 x2))
     (Math/abs (unchecked-subtract-int y1 y2))))

(defn- closest [target candidate1 candidate2 dist-fn]
  #_(tap> [target candidate1 candidate2])
  (let [d1 (dist-fn target (undraw candidate1))
        d2 (dist-fn target (undraw candidate2))]
    (cond (= d1 d2) [:draw candidate1 candidate2]
          (< d1 d2) candidate1
          (> d1 d2) candidate2)))

(defn- next-nodes [left right go-left?]
  (cond (every? nil? [left right]) []
        (not left) [right]
        (not right) [left]
        :else (if go-left? [left right] [right left])))

(defn nn-search [{:keys [left right value dimension]} target dist-fn]
  (if (every? nil? [left right]) value
      (let [[next-node other-node]
            (next-nodes left right (< (nth target dimension)
                                      (nth value dimension)))

            best (closest target value (nn-search next-node target dist-fn) dist-fn)]

        (if (and other-node (>= (dist-fn (undraw best) target)
                                (dist-fn value target)))

          (closest target best (nn-search other-node target dist-fn) dist-fn)
          best))))