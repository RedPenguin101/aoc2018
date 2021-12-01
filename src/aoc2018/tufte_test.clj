(ns aoc2018.tufte-test)

(require '[taoensso.tufte :as tufte :refer (defnp p profiled profile)])

(tufte/add-basic-println-handler! {})

;;; Let's define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

;; How do these fns perform? Let's check:

(profile ; Profile any `p` forms called during body execution
 {} ; Profiling options; we'll use the defaults for now
 (dotimes [_ 5]
   (p :get-x (get-x))
   (p :get-y (get-y))))