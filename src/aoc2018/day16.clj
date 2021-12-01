(ns day16
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.set :as set]))

"
 four registers 0-3 - initialize at 0
 16 instructions/opcodes 0-15
 intruction: [opcode input-a input-b output-c]
 value A -> take value literally
 register A -> take value in register A [0,1,2,3]
 (output is always treated as register)

 intst: 
 addr addi 
 mulr muli 
 banr bani 
 borr bori 
 setr seti 
 gtir gtri gtrr
 eqir eqri eqrr

 need to infer the instruction from the opcode
 
Before: [1, 2, 3, 2]
3 1 3 0
After:  [1, 2, 3, 2]
 
  how many samples in your puzzle input behave like three or more opcodes?
 
 CONCEPTS
 Registers - 4 tuple
 Instruction (Opcode, input a, input b, output) - 4 tuple
 Value / immediate
 Sample: see below
 apply-instruction-x: fn instruction registers -> registers
 candidates: fn before, after, instruction -> set instruction-names

 
 In the opcode descriptions below, if something says value A, it means to take the number given as A literally. (This is also called an immediate value.) If something says register A, it means to use the number given as A to read from (or write to) the register with that number. So, if the opcode addi adds register A and value B, storing the result in register C, and the instruction addi 0 7 3 is encountered, it would add 7 to the value contained by register 0 and store the sum in register 3, never modifying registers 0, 1, or 2 in the process.
 "

(defn parse [sample] (map vec (partition 4 (map #(Integer/parseInt %) (re-seq #"\d+" sample)))))

(def input (map parse (str/split (first (str/split (slurp "resources/day16input") #"\n\n\n")) #"\n\n")))

(def test-prog (map vec (partition 4 (map #(Integer/parseInt %) (re-seq #"\d+" (second (str/split (slurp "resources/day16input") #"\n\n\n\n")))))))

(defn addr [[_ a b c] r] (assoc r c (+ (nth r a) (nth r b))))
(defn addi [[_ a b c] r] (assoc r c (+ (nth r a) b)))
(defn mulr [[_ a b c] r] (assoc r c (* (nth r a) (nth r b))))
(defn muli [[_ a b c] r] (assoc r c (* (nth r a) b)))
(defn banr [[_ a b c] r] (assoc r c (bit-and (nth r a) (nth r b))))
(defn bani [[_ a b c] r] (assoc r c (bit-and (nth r a) b)))
(defn borr [[_ a b c] r] (assoc r c (bit-or (nth r a) (nth r b))))
(defn bori [[_ a b c] r] (assoc r c (bit-or (nth r a) b)))
(defn setr [[_ a _ c] r] (assoc r c (nth r a)))
(defn seti [[_ a _ c] r] (assoc r c a))
(defn gtir [[_ a b c] r] (assoc r c (if (> a (nth r b)) 1 0)))
(defn gtri [[_ a b c] r] (assoc r c (if (> (nth r a) b) 1 0)))
(defn gtrr [[_ a b c] r] (assoc r c (if (> (nth r a) (nth r b)) 1 0)))
(defn eqir [[_ a b c] r] (assoc r c (if (= a (nth r b)) 1 0)))
(defn eqri [[_ a b c] r] (assoc r c (if (= (nth r a) b) 1 0)))
(defn eqrr [[_ a b c] r] (assoc r c (if (= (nth r a) (nth r b)) 1 0)))

(def ops [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn candidates [before instruction after]
  (keep #(when (= after (try (% instruction before) (catch IndexOutOfBoundsException _ nil))) %) ops))

(def init-poss
  (zipmap (range 0 16) (repeat (set ops))))

(defn narrow [possibilities sample]
  (let [opcode (first (second sample))]
    (update possibilities opcode set/intersection (set (apply candidates sample)))))

(defn run [samples init]
  (reduce narrow init samples))

(defn eliminate-found [possibilities]
  (let [new (into {} (let [found (apply set/union (filter #(= 1 (count %)) (vals possibilities)))]
                       (for [[num fns] possibilities]
                         (if (= 1 (count fns)) [num fns]
                             [num (set/difference fns found)]))))]
    (if (= new possibilities)
      (into {} (map (fn [[k v]] [k (first v)]) new))
      (recur new))))

(defn compute [fns]
  (fn [register instruction]
    ((fns (first instruction)) instruction register)))

(comment
  (time (reduce (compute (eliminate-found (run input init-poss)))
                [0 0 0 0]
                test-prog)))

(deftest candidates-test
  (is (= 592  (count (filter #(> % 2) (map (comp count #(apply candidates %)) input)))))
  (is (= 3 (count (candidates [3 2 1 1] [9 2 1 2] [3 2 2 1])))))

(deftest instr
  (is (= [12 13 (+ 13 14) 15]
         (addr [0 1 2 2] [12 13 14 15])))
  (is (= [3 2 2 1]
         (addi [9 2 1 2] [3 2 1 1])
         (mulr [9 2 1 2] [3 2 1 1])
         (seti [9 2 1 2] [3 2 1 1]))))