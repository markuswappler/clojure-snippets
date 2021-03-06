(ns clojure-snippets.euler
  (:require [clojure.edn :as edn]
            [clojure.math.numeric-tower :as numeric]
            [clojure-snippets.math :as math]
            [clojure-snippets.util :as util]
            [clojure-snippets.date :as date]))

(defn- slurp-numbers [file]
  (->> (slurp file)
    (clojure.string/split-lines)
    (map edn/read-string)))

(defn- slurp-digits [file]
  (->> (slurp file)
    clojure.string/split-lines
    clojure.string/join
    (map (comp read-string str))))

(defn- slurp-matrix
  ([file sep]
    (slurp-matrix file sep (comp
                             read-string
                             (fn [entry] 
                               (if (= \0 (first entry)) 
                                 (subs entry 1) 
                                 entry)))))
  ([file sep parse-entry]
    (let [parse-line (fn [line]
                       (->> (clojure.string/split line sep) 
                         (map parse-entry)
                         vec))]
      (->> (slurp file)
        clojure.string/split-lines
        (map parse-line)
        vec
        util/make-matrix))))

(defn pandigital? [s]
  (if (and (= 10 (count s))
           (re-find #"0" (subs s 1))
           (re-find #"1" s)
           (re-find #"2" s)
           (re-find #"3" s)
           (re-find #"4" s)
           (re-find #"5" s)
           (re-find #"6" s)
           (re-find #"7" s)
           (re-find #"8" s)
           (re-find #"9" s))
    true
    false))

;; PROBLEM 1

(defn solve-1 
  ([] (solve-1 1000))
  ([n] 
    (reduce + (for [k (range 3 n)
                    :when (or (zero? (mod k 3))
                              (zero? (mod k 5)))]
                k))))

(defn solve-1-sample 
  ([] (solve-1-sample 1000))
  ([n]
    (let [sum-div-by (fn [d]
                       (let [k (quot (dec n) d)]
                         (quot (* d k (inc k)) 2)))]
      (+ (sum-div-by 3)
         (sum-div-by 5)
         (- (sum-div-by 15))))))

;; PROBLEM 2
;; Every third Fibonacci number is even: 2, 8, 34, 144, ...
;; fk = 4 * fk-3 + fk-6 

(defn solve-2 
  ([] (solve-2 (int 4e6)))
  ([n]
    (->> ((math/make-fib 1 4) 2 8)
      (take-while (fn [a] (>= n a)))
      (reduce +))))

;; PROBLEM 3

(defn solve-3
  ([] (solve-3 (solve-3 600851475143 1000)))
  ([n] (solve-3 n n))
  ([n max-p]
    (let [div (fn [n p] (if (zero? (rem n p))
                          (recur (quot n p) p)
                          n))]
      (loop [n n ps (math/primes max-p)]
        (if-let [p (first ps)]
          (let [d (div n p)]
            (if (= 1 d)
              n
              (recur d (rest ps))))
          n)))))

;; PROBLEM 4

(defn solve-4
  ([] (solve-4 3))
  ([n]
    (let [lower (numeric/expt 10 (dec n))
          upper (numeric/expt 10 n)  
          palindrome? (fn [k]
                        (let [fwd (str k)
                              bwd (apply str (reverse fwd))]
                          (= fwd bwd)))]
      (apply max (for [k0 (range lower upper)
                       k1 (range lower (inc k0))
                       :let [prod (* k0 k1)]
                       :when (palindrome? prod)]
                   prod)))))

;; PROBLEM 5
;; compute for each prime at most 20 the maximum power at most 20
;; this number enforces the maximal occurence of that prime

(defn solve-5 
  ([] (solve-5 20))
  ([n]
    (let [pow (fn [k] (->> (iterate (partial * k) k)
                        (take-while (partial >= n))
                        (last)))]
      (->> (math/primes n)
        (map pow)
        (reduce *)))))

;; PROBLEM 6

(defn solve-6 
  ([] (solve-6 100))
  ([n]
    (let [nums (math/range-sum n)
          sqrs (quot (* nums (inc (* 2 n))) 3)]
      (- (* nums nums) sqrs))))

;; PROBLEM 7
;; The nth prime is about n * log(n).
;; Thus, take 2n * (log(n) + 1) as upper bound

(defn solve-7
  ([] (solve-7 (inc (int 1e4))))
  ([n]
    (nth (math/primes (* 2 n (inc (numeric/ceil (Math/log n))))) 
         (dec n))))

;; PROBLEM 8

(defn solve-8 
  ([] (solve-8 (slurp-digits "resources/euler-8.txt")))
  ([digits]
    (reduce max (map * 
                     digits 
                     (drop 1 digits) 
                     (drop 2 digits) 
                     (drop 3 digits) 
                     (drop 4 digits)))))

;; PROBLEM 9

(defn solve-9
  ([] (solve-9 1000))
  ([n]
    (first (for [a (range 1 n)
                 b (range (inc a) n)
                 :let [c (- n a b)]
                 :when (< b c)
                 :when (= (* c c) (+ (* a a) (* b b)))]
             (* a b c)))))

;; PROBLEM 10

(defn solve-10
  ([] (solve-10 (int 2e6)))
  ([n] (reduce + (math/primes (dec n)))))

;; PROBLEM 11

(defn solve-11 
  ([] (solve-11 (slurp-matrix "resources/euler-11.txt" #"\s")))
  ([matrix]
    (let [row (fn [i j] (for [k (range 4)] [i (+ j k)]))
          col (fn [i j] (for [k (range 4)] [(+ i k) j]))
          diag1 (fn [i j] (for [k (range 4)] [(+ i k) (+ j k)]))
          diag2 (fn [i j] (for [k (range 4)] [(- i k) (+ j k)]))
          prod (fn [ijs] (->> ijs
                           (map (fn [[i j]] (if (and (<= 0 i) 
                                                     (> (matrix :rows) i) 
                                                     (<= 0 j) 
                                                     (> (matrix :cols) j))
                                              (matrix :entry i j)
                                              0)))
                           (reduce *)))]
      (->> (for [i (range (matrix :rows))
                 j (range (matrix :cols))]
             [(prod (row i j))
              (prod (col i j))
              (prod (diag1 i j))
              (prod (diag2 i j))])
        (reduce concat)
        (reduce max)))))

;; PROBLEM 12
;; tau(k) := #{d: 1<=d<=k, d|k} 
;; k-th triangle number is k*(k+1)/2
;; k even: k/2 and k+1 are coprime => number of factors is tau(k/2)*tau(k+1)
;; k odd: (k+1)/2 and k are coprime => number of factors is tau((k+1)/2)*tau(k)

(defn solve-12 
  ([] (solve-12 500))
  ([n]
  (let [divisors (fn [k]
                   (if (even? k)
                     (* (math/tau (quot k 2)) (math/tau (inc k)))
                     (* (math/tau (quot (inc k) 2)) (math/tau k))))]
    (loop [k 2 triangle-number 1]
      (let [tn (+ triangle-number k)
            d (divisors k)]
        (if (< n d)
          tn
          (recur (inc k) tn)))))))

;; PROBLEM 13

(defn solve-13 [] 
  (->> (slurp-numbers "resources/euler-13.txt")
    (reduce +)
    str
    (take 10)
    (apply str)))

;; PROBLEM 14
;; one has only to consider numbers between n/2 and n,
;; others follow in a longer Collatz Sequence starting at
;; twice its value
;; step 1: foreach remaining value go through the sequence
;; until a value in the given intervall is found
;; this can be neglected
;; step 2: compute the length of the sequence of the
;; (far less) remaining values

(defn solve-14
  ([] (solve-14 (int 1e6)))
  ([n]
    (let [candidates (int-array n 0)
          successor (fn [k]
                      (->> (rest (math/collatz k))
                        (drop-while #(<= n %))
                        first))]
      (do
        (doseq [k (range (quot n 2) n)]
          (aset candidates (successor k) -1))
        (doseq [k (range (quot n 2) n)
                :when (zero? (aget candidates k))]
          (aset candidates k (count (math/collatz k))))
        (util/amax candidates)))))

;; PROBLEM 15
;; make 2n decisions: right or down
;; choose n down-decisions from all decisions

(defn solve-15 
  ([] (solve-15 20))
  ([n] (math/binom (* 2 n) n)))

;; PROBLEM 16

(defn solve-16
  ([] (solve-16 1000))
  ([n]
    (->> (numeric/expt 2 n)
      str
      (map str)
      (map edn/read-string)
      (reduce +))))

;; PROBLEM 17

(defn word-number [k]
  (condp = k
    0 ""
    1 "one"
    2 "two"
    3 "three"
    4 "four"
    5 "five"
    6 "six"
    7 "seven"
    8 "eight"
    9 "nine"
    10 "ten"
    11 "eleven"
    12 "twelve"
    13 "thirteen"
    14 "fourteen"
    15 "fifteen"
    16 "sixteen"
    17 "seventeen"
    18 "eighteen"
    19 "nineteen"
    20 "twenty"
    30 "thirty"
    40 "forty"
    50 "fifty"
    60 "sixty"
    70 "seventy"
    80 "eighty"
    90 "ninety"
    1000 "one thousend"
    (if (> 100 k)
      (let [dec (word-number (* (quot k 10) 10))
            unit (word-number (mod k 10))]
        (clojure.string/join " " [dec unit]))
      (let [cent (clojure.string/join " " [(word-number (quot k 100)) "hundred"])
            dec (word-number (mod k 100))]
        (if (empty? dec)
          cent
          (clojure.string/join " " [cent "and" dec]))))))

(defn solve-17
  ([] (->> (range 1 1001)
        (map solve-17)
        (reduce +)))
  ([k] (-> (word-number k)
         (clojure.string/replace " " "")
         count)))

;; PROBLEM 18

(defn solve-18 
  ([] (solve-18 (slurp-matrix "resources/euler-18.txt" #"\s")))
  ([matrix]
    (let [neighbors (fn [[i j]] [[(inc i) j] [(inc i) (inc j)]])
          dist (fn [_ [i j]] (- 100 (matrix :entry i j)))
          dijkstra (util/make-dijkstra neighbors dist)
          source [0 0]
          terminals (for [col (range (matrix :rows))] 
                      [(dec (matrix :rows)) col])
          path (dijkstra [source] terminals)]
      (+ (apply (partial matrix :entry) source) 
         (- (* 100 (dec (matrix :rows))) 
            ((first path) :dist))))))

;; PROBLEM 19

(defn solve-19 []
  (->> (for [month (range 1 13)
             year (range 1901 2001)]
         (date/weekday [1 month year]))
    (filter #{:sunday})
    count))

;; PROBLEM 20

(defn solve-20
  ([] (solve-20 100))
  ([n]
    (->> (inc n)
      (range 1)
      (reduce *')
      (str)
      (map (comp read-string str))
      (reduce +))))

;; PROBLEM 25

(defn solve-25 
  ([] (solve-25 1000))
  ([digits]
    (let [bound (numeric/expt 10 (dec digits))
          fib ((math/make-fib 1 1) 0N 1N)]
      (->> fib
        (map-indexed (fn [i f] [i f]))
        (drop-while (fn [[i f]] (> bound f)))
        first
        first))))

;; PROBLEM 67

(defn solve-67 [] 
  (solve-18 (slurp-matrix "resources/euler-67.txt" #"\s")))

;; PROBLEM 81

(defn solve-81 
  ([] (solve-81 (slurp-matrix "resources/euler-81.txt" #",")))
  ([matrix]
    (let [neighbors (fn [[i j]]
                      (util/cond-vector 
                        (> (matrix :rows) (inc i)) [(inc i) j]
                        (> (matrix :cols) (inc j)) [i (inc j)]))
          dist (fn [_ [i j]] (matrix :entry i j))
          dijkstra (util/make-dijkstra neighbors dist)
          source [0 0]
          terminal [(dec (matrix :rows)) (dec (matrix :cols))]
          path (dijkstra [source] [terminal])]
      (+ (apply (partial matrix :entry) source)
         ((first path) :dist)))))

;; PROBLEM 82

(defn solve-82
  ([] (solve-82 (slurp-matrix "resources/euler-82.txt" #",")))
  ([matrix]
    (let [neighbors (fn [[i j]]
                      (util/cond-vector 
                        (and (<= 0 (dec i)) (<= 0 j)) [(dec i) j]
                        (and (> (matrix :rows) (inc i)) (<= 0 j)) [(inc i) j]
                        (> (matrix :cols) (inc j)) [i (inc j)]))
          dist (fn [_ [i j]] (matrix :entry i j))
          dijkstra (util/make-dijkstra neighbors dist)
          sources (for [i (range (matrix :rows))] 
                    [i -1])
          terminals (for [i (range (matrix :rows))] 
                      [i (dec (matrix :cols))])
          path (dijkstra sources terminals)]
      ((first path) :dist))))

;; PROBLEM 83

(defn solve-83
  ([] (solve-83 (slurp-matrix "resources/euler-83.txt" #",")))
  ([matrix]
    (let [neighbors (fn [[i j]]
                      (util/cond-vector 
                        (<= 0 (dec i)) [(dec i) j]
                        (> (matrix :rows) (inc i)) [(inc i) j]
                        (<= 0 (dec j)) [i (dec j)]
                        (> (matrix :cols) (inc j)) [i (inc j)]))
          dist (fn [_ [i j]] (matrix :entry i j))
          dijkstra (util/make-dijkstra neighbors dist)
          source [0 0]
          terminal [(dec (matrix :rows)) (dec (matrix :cols))]
          path (dijkstra [source] [terminal])]
      (+ (apply (partial matrix :entry) source)
         ((first path) :dist)))))

;; PROBLEM 104

;; brute force - proves to be far too slow

(defn solve-104-slow []
  (let [fib ((math/make-fib 1 1) 0N 1N)
        pd? (fn [f]
              (let [s (str f)
                    len (count s)
                    fst (subs s 0 (min len 9))
                    lst (subs s (max 0 (- len 9)))]
                (and
                  (pandigital? (str fst "0"))
                  (pandigital? (str lst "0")))))]
    (->> fib
      (map-indexed (fn [i f] [i f]))
      (drop-while (fn [[i f]] (not (pd? f))))
      first
      first)))

;; adequate solution
;; iterate through the Fibonacci sequence modulo 10^9 for the
;; last digits; in case of a match compute the fibonacci number
;; by fast computation via the index and check if the
;; first digits match too.

(defn solve-104 []
  (let [m (int 1e9)]
    (loop [idx 1 f0 0 f1 1]
      (if (and (pandigital? (str f1 "0"))
               (let [fst (subs (str (math/fib idx)) 0 9)]
                 (pandigital? (str fst "0"))))
        idx
        (recur (inc idx) f1 (mod (+ f0 f1) m))))))

;; PROBLEM 107

(defn solve-107
  ([] (solve-107 (slurp-matrix "resources/euler-107.txt" 
                               #"," 
                               (fn [entry]
                                 (when (not= "-" entry)
                                   (read-string entry))))))
  ([matrix]
    (let [edges (for [i (range (matrix :rows))
                      j (range i)
                      :let [weight (matrix :entry i j)]
                      :when weight]
                  {:node-1 i :node-2 j :weight weight})
          total-weight (fn [edges] (reduce + (map :weight edges)))]
      (- (total-weight edges) 
         (total-weight (util/kruskal edges))))))

;; PROBLEM 206

(defn solve-206 []
  (let [placeholder (bigint (bigdec 1e19))
        vary (fn [pos num]
               (let [dec (numeric/expt 10 pos)]
                 (map (fn [dgt] (+ num (* dgt dec))) (range 10))))
        match (fn [len pattern num]
                (let [regex (-> pattern
                              (clojure.string/replace "_" "\\d")
                              re-pattern)
                      sqr (str (* num num))
                      s (if (= :all len)
                          sqr
                          (subs sqr (- (count sqr) len)))]
                  (re-matches regex s)))]
    (->> [placeholder]
      (mapcat (partial vary 0))
      (filter (partial match 1 "0"))
      (mapcat (partial vary 1))
      (mapcat (partial vary 2))
      (filter (partial match 3 "9_0"))
      (mapcat (partial vary 3))
      (mapcat (partial vary 4))
      (filter (partial match 5 "8_9_0"))
      (mapcat (partial vary 5))
      (mapcat (partial vary 6))
      (filter (partial match 7 "7_8_9_0"))
      (mapcat (partial vary 7))
      (mapcat (partial vary 8))
      (filter (partial match 9 "6_7_8_9_0"))
      (mapcat (partial vary 9))
      (map #(- % placeholder))
      (filter (partial match :all "1_2_3_4_5_6_7_8_9_0"))
      first)))
    
;; PROBLEM 297
;; s(n) := sum mu(k), 1<k<n.
;; One can obtain the following recursion:
;; Let f be the greatest Fibonacci number smaller than k.
;; s(1) = 0, s(2) = 1, s(k) = s(f) + s(k-f) + k-f.
;; Step 1: Compute s(k) for the Fibonacci sequence via
;;         s(fk) = s(fk-1) + s(fk-2) + fk-2
;; Step 2: Use the recursion to compute s(n).

(defn solve-297 
  ([] (solve-297 (long 1e17)))
  ([n]
    (let [terms (loop [a 1 b 2 res {1 0 2 1}]
                  (let [nxt (+ a b)]
                    (if (> n nxt)
                      (recur b nxt (assoc res nxt (+ a (res a) (res b))))
                      res)))
          next-smaller-key (fn [m]
                             (->> terms
                               keys
                               (filter #(> m %))
                               (apply max)))]
      (loop [res 0 m n]
        (if (= 1 m)
          res
          (let [k (next-smaller-key m)
                p (- m k)]
            (recur (+ res (terms k) p) p)))))))

;; PROBLEM 351
;; use symmetry and compute the solution for a sector of 1/6 of the shape
;; consider e.g. the "spoke" to the right corner (inclusive) and the sector above
;; up to the "spoke" to the right up corner (exclusive)
;; each point is uniquely determined by coordinates (m,k), 0<=m<k, 0<=k<=n.
;; k is the number of steps along the "spoke" to the right
;; m is the number of steps then following into the direction 
;; parallel to the side face from the right corner to the right up corner
;; the points that are not hidden are exactly the points 
;; (0,0),(0,1) and all (m,k) with 1<=m<k<=n and m,k are coprime

;; first try
;; compute the hidden points of (0,1) (the points "behind" (0,1)
;; compute the coprimes generated by (1,2) and the related hidden points
;; compute the coprimes generated by (1,3) and the related hidden points
;; sum all up
;; this is far too slow due to the exponential explosion of coprime-pairs
;; even though this approach may be speeded up by using more symmetry in the sector
;; ((m,k) has as many hidden points as (k-m,k))

(defn solve-351-slow
  ([] (solve-351-slow (int 1e8)))
  ([n]
    (let [hidden (fn [k] (dec (quot n k)))
          hidden-mk (fn [m k]
                      (->> (math/coprimes m k (fn [_ k] (pos? (hidden k))))
                        (map (fn [[_ k]] (hidden k)))
                        (reduce +)))
          hidden-12 (future (hidden-mk 1 2))
          hidden-13 (future (hidden-mk 1 3))]
      (* 6 (+ (hidden 1) @hidden-12 @hidden-13)))))

;; second try
;; subtract the number of not hidden points (except the center point) from the
;; total number of points (except the center)
;; total number is 6+12+18+... (go layer by layer from inner to outer)
;; number of not hidden points 6 times the number of points with 
;; coordinates (m,k), 0<=m<k, 0<k<=n and m,k coprime
;; but this is the sum of Euler's totient function from 1 to n
;; conveniently fast solution, about 2 minutes on dual core, 2.2 GHz

(defn solve-351 
  ([] (solve-351 (int 1e8)))
  ([n]
    (let [total (* 6 (math/range-sum n))
          not-hidden (* 6 (math/phi-summatory n))]
      (- total not-hidden))))