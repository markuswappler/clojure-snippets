(ns clojure-snippets.math
  (:require [clojure-snippets.tree-traversal :as traversal]))

(defn make-fib 
  "Returns a function that takes two arguments [a0 a1] and
  returns a lazy (generalized Fibonacci) sequence of 
  a0, a1, (+ (* c0 a0) (* c1 a1)), 
  (+ (* c0 a1) (* c1 (+ (* c0 a0) (* c1 a1)))) etc."
  [c0 c1]
  (fn fib [a0 a1] 
    (lazy-seq 
      (cons a0 (fib a1 (+ (* c0 a0) (* c1 a1)))))))

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
        walk (traversal/make-walk :depth :pre 
                                  (constantly true) identity identity 
                                  children)]
    (walk [1 2] [1 3])))