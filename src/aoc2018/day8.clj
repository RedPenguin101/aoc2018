(ns aoc2018.day8
  (:require [clojure.string :as str]))

(def example (list 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))
(def input (map #(Long/parseLong %) (str/split (str/trim-newline (slurp "resources/day8input")) #" ")))

(defn build-tree [[child-count meta-count & tape]]
  (first (letfn [(build [{:keys [children child-count meta-count]
                          :as root-node}
                         remaining-tape]
                   (if
                    (= (count children) child-count)
                     [(assoc root-node :meta (take meta-count remaining-tape))
                      (drop meta-count remaining-tape)]

                     (let [[child tape] (next-child remaining-tape)]
                       (recur (update root-node :children conj child)
                              tape))))

                 (next-child [[child-count meta-count & tape]]
                   (let [child {:child-count child-count :meta-count meta-count}]
                     (if (zero? child-count)
                       [(assoc child :meta (take meta-count tape))
                        (drop meta-count tape)]

                       (build child tape))))]

           (build
            {:child-count child-count
             :meta-count meta-count}
            tape))))

(comment
  (=  (build-tree '(0 1 99))
      {:child-count 0, :meta-count 1, :meta '(99)})

  (= (build-tree '(1 1 0 1 99 2))
     {:child-count 1, :meta-count 1
      :children '({:child-count 0, :meta-count 1, :meta (99)})
      :meta '(2)})

  (= (build-tree (list 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))
     {:child-count 2
      :meta-count 3
      :children
      '({:child-count 1, :meta-count 1, :children ({:child-count 0, :meta-count 1, :meta (99)}), :meta (2)}
        {:child-count 0, :meta-count 3, :meta (10 11 12)})
      :meta '(1 1 2)}))


(comment

  (build-tree input) ;; probably don't run this, output is large 
  )

(defn count-meta [root-node]
  (concat (:meta root-node)
          (mapcat count-meta (:children root-node))))

(comment
  (count-meta (build-tree example))
  (def tree (build-tree input))
  (reduce + (count-meta tree))
  ;; => 37439
  )

(defn value [root]
  (if (zero? (:child-count root))
    (reduce + (:meta root))
    (->> (:meta root)
         (keep #(nth (reverse (:children root)) (dec %) nil)) ;; ugly reverse here because of implementation as list
         (map value)
         (reduce +))))

(comment
  (value (build-tree example))
  ;; => 66

  (value tree)
  ;; => 20815
  )

