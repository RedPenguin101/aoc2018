(ns aoc2018.day19
  (:require [clojure.test :refer [deftest is are]]
            [clojure.string :as str]))

"Day 19: Go with the flow
 
 Same virtual machine as day 16, with 6 registers
 opcodes, flow control (jump), instruction pointer, register
 Instruction pointer can be bound to a register
 setr/i & addr/i can act as absolute/relative jumps respectively - others can have weird effects
 1 register designated the 'instruction pointer'. Starts at 0 and increments on each operation

 Terms: Program, Register, Instruction (4tuple), OpCode, Value/Immediate, 
 Instruction Pointer (IP), Instruction Register (IR), Instruction Register Value (IRV), Jump

 EXAMPLE

 Program:
 #ip 0
 i0 = seti 5 0 1
 i1 = seti 6 0 2
 i2 = addi 0 1 0
 i3 = addr 1 2 3
 i4 = setr 1 0 0
 i5 = seti 8 0 4
 i6 = seti 9 0 5

 WHAT IS THE VALUE AT r0 when the program terminates?


 First, set IR to 0
 
         ip  registers          instruction  new-registers
 Init:   0   [0, 0, 0, 0, 0, 0] [seti 5 0 1] [0, 5, 0, 0, 0, 0]
 4. set IRV to IP (0)
 2. IP = 0, so execute i0 = [seti 5 0 1]: set r1 to 5
 2. set IP to IRV (1)
 3. Increment IP (2)
 
 Step1: 1 [1, 5, 0, 0, 0, 0] [seti 6 0 2] [1, 5, 6, 0, 0, 0]
 4. set IRV to IP (1)
 1. IP=1, so execute i1 = [seti 6 0 2], set r2 to 6
 2. set IP to IRV (1)
 3. Increment IP (2)
 
 Step 2: 2 [2, 5, 6, 0, 0, 0] [addi 0 1 0] [3, 5, 6, 0, 0, 0]
 4. set IRV to IP (2)
 1. IP=2, so execute i2 = [addi 0 1 0], add r0 and 1 (2 + 1 = 3) put to r0
 2. set IP to IRV (3)
 3. Increment IP (4)

 Step 3: 4 [4, 5, 6, 0, 0, 0] [setr 1 0 0] [5, 5, 6, 0, 0, 0]
 4. set IRV to IP (4)     
 1. IP=4, so execute i4 = [setr 1 0 0], set r0 to r1 (5)
 2. set IP to IRV (5)
 3. Increment IP (6)

 6 [6, 5, 6, 0, 0, 0] [seti 9 0 5] [6, 5, 6, 0, 0, 9]
 4. set IRV to IP (6)      
 1. IP=6, so execute i6 = [seti 9 0 5], set r5 to 9
 2. set IP to IRV (6)
 3. Increment IP (7)

 IP=7, outside of instruction range, so terminate and return registers

 Concepts:
 HIGH: 
 * run-program :: instruction register, instructions -> final register
 * run-to-termination :: ip, ir, instructions, register -> final register (loop)

 MID: 
 step :: ir, instructions, registers -> next-register
 execute instruction :: instr, registers -> register
 "

;; instructions copied from day 16
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

(def ops (zipmap ["addr" "addi" "mulr" "muli" "banr" "bani" "borr" "bori" "setr" "seti" "gtir" "gtri" "gtrr" "eqir" "eqri" "eqrr"]
                 [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr]))

(defn execute-instruction [registers instr]
  ((ops (first instr)) instr registers))

(defn step
  "Sets the IRV to the IP, and executes instruction IP"
  [instructions ir ip registers]
  (-> registers
      (assoc ir ip)
      (execute-instruction (get instructions ip))))

(defn run
  ([ir instructions] (run ir instructions 0 [0 0 0 0 0 0] 0))
  ([ir instructions ip registers it]
   (cond (>= ip (count instructions)) registers
         (> it 100000000) (do (tap> {:ir ir :ip ip :reg registers
                                     :instr (get instructions ip)})
                              :break)
         :else (let [new-reg (step instructions ir ip registers)]
                 (recur ir instructions (inc (get new-reg ir)) new-reg (inc it))))))

(defn atom-parse [str]
  (try (bigint (Long/parseLong str))
       (catch Exception _e str)))

(defn parse [string]
  (let [[ir & instr] (str/split-lines string)]
    [(Long/parseLong (re-find #"\d+" ir))
     (mapv #(mapv atom-parse (str/split % #" ")) instr)]))

(comment
  (let [[ir instr] (parse (slurp "resources/day19input"))]
    (run ir instr))
  (time (let [[ir instr] (parse (slurp "resources/day19input"))]
          (run ir instr 0 [1 0 0 0 0 0] 0)))
  "Can't brute force it :(")

(defn run-iter [ir instructions]
  (fn [[ip reg]]
    (if (>= ip (count instructions)) [:break reg]
        (let [new-reg (step instructions ir ip reg)]
          [(inc (get new-reg ir)) new-reg]))))

(def example-instr [["seti" 5 0 1] ["seti" 6 0 2] ["addi" 0 1 0] ["addr" 1 2 3] ["setr" 1 0 0] ["seti" 8 0 4] ["seti" 9 0 5]])

(comment

  (def instr (second (parse (slurp "resources/day19input"))))

  (take 500 (iterate (run-iter 4 instr) [0 [0 0 0 0 0 0]]))

  (time (apply merge-with conj {:r0 [] :r4 []}
               (map (fn [[_ [r0 r1 r2 r3 r4 r5]]] (zipmap [:r0 :r4] [r0 r4]))
                    (take 700 (iterate (run-iter 4 instr) [0 [0 0 0 0 0 0]]))))))

;; tests

(deftest step-test
  (let [instr [["seti" 5 0 1] ["seti" 6 0 2] ["addi" 0 1 0] ["addr" 1 2 3] ["setr" 1 0 0] ["seti" 8 0 4] ["seti" 9 0 5]]
        ir 0]
    (are [ip reg result] (= result (step instr ir ip reg))
      0 [0, 0, 0, 0, 0, 0] [0, 5, 0, 0, 0, 0]
      1 [1, 5, 0, 0, 0, 0] [1, 5, 6, 0, 0, 0]
      2 [2, 5, 6, 0, 0, 0] [3, 5, 6, 0, 0, 0]
      4 [4, 5, 6, 0, 0, 0] [5, 5, 6, 0, 0, 0]
      6 [6, 5, 6, 0, 0, 0] [6, 5, 6, 0, 0, 9])
    (is (= [6 5 6 0 0 9] (run ir instr)))))

(deftest execute-instruction-test
  (is (= [0 5 0 0 0 0] (execute-instruction [0 0 0 0 0 0] ["seti" 5 0 1]))))