(ns clojure-snippets.euler
  (:require [clojure.math.numeric-tower :as numeric]
            [clojure-snippets.math :as math]
            [clojure-snippets.util :as util]))

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
    (->> 
      ((math/make-fib 1 4) 2 8)
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
  ([] (solve-8 (->> (slurp "resources/euler-8.txt")
                 (clojure.string/split-lines)
                 (clojure.string/join))))
  ([stream]
    (let [digits (fn [shift]
                   (->> stream
                     (map (comp read-string str))
                     (drop shift)))]
      (apply max (map * (digits 0) (digits 1) (digits 2) (digits 3) (digits 4))))))

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

;; PROBLEM 15
;; make 2n decisions: right or down
;; choose n down-decisions from all decisions

(defn solve-15 
  ([] (solve-15 20))
  ([n] (math/binom (* 2 n) n)))

;; PROBLEM 18

(defn solve-18 
  ([]
    (let [parse-line (fn [line]
                       (->> (clojure.string/split line #"\s") 
                         (map #(if (= \0 (first %)) (subs % 1) %))
                         (map read-string)
                         vec))
          rows (->> (slurp "resources/euler-18.txt")
                 (clojure.string/split-lines)
                 (map parse-line)
                 vec)]
      (solve-18 rows)))
  ([rows]
    (let [cnt (count rows)
          number (fn [[i j]] ((rows i) j))
          neighbors (fn [[i j]] [[(inc i) j] [(inc i) (inc j)]])
          dist (fn [_ pos] (- 100 (number pos)))
          dijkstra (util/make-dijkstra neighbors dist)
          source [0 0]
          terminals (for [col (range cnt)] [(dec cnt) col])
          path (dijkstra [source] terminals)]      
      (+ (number [0 0]) (- (* 100 (dec cnt)) ((first path) :dist))))))

;; PROBLEM 67

(defn solve-67 []
  (let [parse-line (fn [line]
                     (->> (clojure.string/split line #"\s") 
                       (map #(if (= \0 (first %)) (subs % 1) %))
                       (map read-string)
                       vec))
        rows (->> (slurp "resources/euler-67.txt")
               (clojure.string/split-lines)
               (map parse-line)
               vec)]
    (solve-18 rows)))         

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