(ns aoc2018.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [ubergraph.core :as uber]))

(def example
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defn parse-step [line]
  (let [[_ from to] (re-matches
                     #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
                     line)]
    [from to]))

(def graph (apply uber/digraph (map parse-step (str/split-lines example))))

(defn roots [graph]
  (set/difference (set (uber/nodes graph))
                  (set (map :dest (uber/edges graph)))))

(defn all-parents-open? [graph open query]
  (set/subset? (set (uber/predecessors graph query)) (set open)))

(defn reverse-difference
  "To keep the pipe in the next function nice and clean."
  [s1 s2]
  (set/difference s2 s1))

(defn candidates [graph open]
  (->> open
       (mapcat #(uber/find-edges graph {:src %}))
       (map :dest)
       (concat (roots graph))
       (set)
       (reverse-difference (set open))
       (filter (partial all-parents-open? graph open))
       (sort)))

(defn order
  ([graph] (order graph []))
  ([graph open]
   (if (= (count open) (uber/count-nodes graph))
     open
     (recur graph (conj open (first (candidates graph open)))))))

(def graph2 (apply uber/digraph (map parse-step (str/split-lines (slurp "resources/day7input")))))

(comment
  (order graph [])
  (uber/pprint graph2)
  (order graph2))

(comment
  "Part 2"
  "Structure like"
  {:second 4 :open ["C" "A"]
   :workers [{:working-on "B" :finished-at 6}
             {:working-on "F" :finished-at 9}]})

(def base-secs 60)

(def times (zipmap (map str "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                   (range 1 27)))

(defn finished? [sec worker]
  (when (= sec (:finished-at worker))
    (:working-on worker)))

(defn idle? [worker]
  (not (:working-on worker)))

(defn occupied? [sec worker]
  (and (not (idle? worker)) (> (:finished-at worker) sec)))

(defn assign [candidates sec workers]
  (:workers
   (reduce
    (fn [A worker]
      (cond
        (occupied? sec worker) (update A :workers conj worker)
        (empty? (:cands A)) (update A :workers conj {})

        :else
        (let [[next-candidate & remaining-candidates] (:cands A)]
          (-> A
              (update :workers conj {:working-on next-candidate
                                     :finished-at (+ sec base-secs (get times next-candidate))})
              (assoc :cands remaining-candidates)))))

    {:cands (sort (set/difference (set candidates) (set (keep :working-on workers))))
     :workers []}

    workers)))

(defn tick [graph sec open workers]
  (let [new-open (concat open (keep (partial finished? sec) (remove idle? workers)))]
    (if (= (count new-open) (uber/count-nodes graph))
      {:open (apply str new-open) :secs sec}
      (recur graph (inc sec) new-open (assign (candidates graph new-open) sec workers)))))

(defn create-workers [graph number-of-workers]
  (take number-of-workers
        (concat (mapv #(hash-map :working-on % :finished-at (+ base-secs (get times %))) (candidates graph []))
                (repeat number-of-workers {}))))

(comment
  (tick graph 10 ["C" "A" "B" "F"]
        [{:working-on "D"
          :finished-at 10}
         {}])

  (tick graph 0 [] (create-workers graph 2))
  (tick graph2 0 [] (create-workers graph2 5))

  (assign ["E"] 10 [{:working-on "D"
                     :finished-at 10}
                    {}])
  1)
