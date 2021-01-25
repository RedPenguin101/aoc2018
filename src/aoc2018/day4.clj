(ns aoc2018.day4
  (:require [clojure.string :as str]
            [tick.alpha.api :as t]))

(def records (sort (str/split-lines (slurp "resources/day4input"))))
(def sample-records (take 100 (sort (str/split-lines (slurp "resources/day4input")))))

(defn shifts [records]
  (->> records
       (partition-by #(= "Guard" (subs % 19 24)))
       (partition 3 2)
       (map #(mapcat concat %))))

(comment
  (first (shifts sample-records))
;; => ("[1518-02-24 23:58] Guard #2347 begins shift"
;;     "[1518-02-25 00:27] falls asleep"
;;     "[1518-02-25 00:58] wakes up"
;;     "[1518-02-25 23:53] Guard #211 begins shift")
  )

(def s (shifts sample-records))

(subs (ffirst s) 1 17)
;; => "1518-02-24 23:58"

(defn timestamp-record [record]
  (let [[_ date time event] (re-matches
                             #"\[(\d{4}-\d{2}-\d{2}) (\d{2}:\d{2})\] (.+)$"
                             record)]
    [(-> (t/date date) (t/at time))
     event]))

(defn summarise-shift [shift]
  (let [guard-id (Long/parseLong (second (re-find #"Guard #(\d+)" (first shift))))]
    [guard-id
     (-> (reduce
          (fn [A [timestamp event]]
            (-> A
                (update (if (:asleep? A) :time-asleep :time-awake)
                        t/+ (t/between (:last-time A)
                                       timestamp))
                (assoc :asleep? (= event "falls asleep"))
                (assoc :last-time timestamp)))
          {:asleep? false
           :time-asleep #time/duration "PT0M"
           :time-awake  #time/duration "PT0M"
           :last-time (first (timestamp-record (first shift)))}
          (map timestamp-record shift))
         (dissoc :asleep?)
         (dissoc :last-time))]))

(defn merge-times [a b]
  (if (nil? a) b
      (merge-with t/+ a b)))

(sort-by (comp :time-asleep second)
         (reduce
          (fn [A [k v]]
            (update A k merge-times v))
          {}
          (map summarise-shift s)))

(defn summaries-shift2 [shift]
  (let [guard-id (Long/parseLong (second (re-find #"Guard #(\d+)" (first shift))))
        naps (->> (partition 2 1 shift)
                  (filter #(= "falls asleep" (subs (first %) 19))))]
    [guard-id (apply concat
                     (for [nap naps]
                       (apply range (map #(Long/parseLong (second (re-find #":(\d+)" %))) nap))))]))


(map-indexed vector (map first s))

(frequencies (mapcat second (map summaries-shift2 [(nth s 13)
                                                   (nth s 17)
                                                   (nth s 24)])))

(mapcat second (map summaries-shift2 [(nth s 13)
                                      (nth s 17)
                                      (nth s 24)]))

"11->52"
"38->48"
" 5->55"

(let [[guard mns]
      (last (sort-by (comp count second)
                     (reduce (fn [A [g mns]]
                               (update A g concat mns))
                             {}
                             (map summaries-shift2 s))))]
  [guard (sort-by second (frequencies mns))])
