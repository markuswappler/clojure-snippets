(ns clojure-snippets.euler
  (:require [clojure.math.numeric-tower :as numeric]
            [clojure-snippets.tree-traversal :as traversal]))

;; helper

(defn integer 
  "integer number via exponential notation"
  [m e]  
  (* m (numeric/expt 10 e)))

(defn fib-fn 
  "Returns a function that takes two arguments [a0 a1] and
  returns a lazy (generalized Fibonacci) sequence of 
  a0, a1, (+ (* c0 a0) (* c1 a1)), 
  (+ (* c0 a1) (* c1 (+ (* c0 a0) (* c1 a1)))) etc."
  [c0 c1]
  (fn [a0 a1]
    (->> 
      (iterate (fn [[a b]] [b (+ (* c0 a) (* c1 b))]) [a0 a1])
      (map first))))

(defn binom 
  "binomial coefficient"
  [n k]
  (if (> k (- n k))
    (binom n (- n k))
    (loop [n n k k acc 1]
      (if (zero? k)
        acc
        (recur n (dec k) (/ (* acc (+ n (- k) 1)) k))))))

(defn coprimes [pred]
  (let [children (fn [[n m]]
                   (->>
                     [[n (+ (* 2 n) m)]
                      [m (- (* 2 m) n)]
                      [m (+ (* 2 m) n)]]
                     (filter (fn [[n m]] (pred n m)))))
        walk (traversal/walk-fn :depth :pre 
                                (constantly true) identity identity 
                                children)]
    (walk [1 2] [1 3])))

;; 1

(defn solve-1 []
  (reduce + (for [k (range 3 1000)
                  :when (or (zero? (mod k 3))
                            (zero? (mod k 5)))]
              k)))

(defn solve-1-sample []
  (let [sum-div-by (fn [n]
                     (let [p (quot 999 n)]
                       (quot (* n p (inc p)) 2)))]
    (+ (sum-div-by 3)
       (sum-div-by 5)
       (- (sum-div-by 15)))))

;; 2
;; Every third Fibonacci number is even: 2, 8, 34, 144, ...
;; fib(k) = 4 * fib(k - 3) + fib(k - 6) 

(defn solve-2 []
  (let [bound (integer 4 6)]
    (->> 
      ((fib-fn 1 4) 2 8)
      (take-while (fn [a] (>= bound a)))
      (reduce +))))

;; 15
;; make 40 decisions: right or down
;; choose 20 down-decisions from all decisions

(defn solve-15 []
  (binom 40 20))

;; 351
;; TODO

(defn solve-351 []
  (let [bound (integer 10 8)]
    (coprimes (fn [n m] (>= bound (+ n m))))))

;; 354
;; TODO

(defn solve-354 [])