(ns clojure-snippets.math
  (:require [clojure-snippets.tree-traversal :as traversal]
            [clojure-snippets.util :as util]))

(defn binom
  "binomial coefficient"
  [n k]
  (if (> k (- n k))
    (binom n (- n k))
    (loop [n n k k acc 1]
      (if (zero? k)
        acc
        (recur n (dec k) (/ (* acc (+ n (- k) 1)) k))))))

(defn range-sum 
  "(range-sum n) 
    = 1 + 2 + ... + (n - 1).
  (range-sum n0 n1) 
    = n0 + (n0 + 1) + ... + (n1 - 1) for n0 < n1,
    = -(n1 + (n1 + 1) + ... + (n0 - 1)) for n0 > n1,
    = 0 for n0 = n1."
  ([n] (quot (* (dec n) n) 2))
  ([n0 n1] (- (range-sum n1) (range-sum n0))))

(defn range-sum-2
  "(range-sum n) 
    = 1 + 3 + 5 + ... + (n - 2) for n odd,
    = 2 + 4 + 6 + ... + (n - 2) for n even.
  (range-sum n0 n1) 
    = n0 + (n0 + 2) + ... + (n1 - 2) for n0 < n1, n0 = n1 (mod 2),
    = -(n1 + (n1 + 2) + ... + (n0 - 2)) for n0 > n1, n0 = n1 (mod 2),
    = 0 for n0 = n1,
    = nil for n1 - n0 = 1 (mod 2)."
  ([n]
    (if (even? n)
      (let [half (quot n 2)]
        (* (dec half) half))
      (let [half (quot (dec n) 2)]
        (* half half))))
  ([n0 n1]
    (when (even? (- n1 n0))
      (- (range-sum-2 n1) (range-sum-2 n0)))))

(defn make-fib 
  "Returns a function that takes two arguments [a0 a1] and
  returns a lazy (generalized) Fibonacci sequence with
  f0 = a0, f1 = a1, fk = c1 * fk-1 + c0 * fk-2."
  [c0 c1]
  (fn fib [a0 a1] 
    (lazy-seq 
      (cons a0 (fib a1 (+ (* c0 a0) (* c1 a1)))))))

;; uses java-array for the sake of efficiency
(defn primes 
  "Lists all prime numbers up to n via the sieve of Eratosthenes."
  [n]
  (if (< 1 n)
    (let [n+1 (inc n)
          ps (boolean-array n+1 true)]
      (doseq [even (range 4 n+1 2)]
        (aset-boolean ps even false))
      (doseq [k (range 3 n+1)]
        (if (aget ps k)
          (doseq [mult (range (* k k) n+1 (* 2 k))]
            (aset-boolean ps mult false))))
      (cons 2 (for [k (range 3 n+1 2)
                    :when (aget ps k)]
                k)))))

(defn coprimes
  "Generates a lazy sequence of pairs of coprime integers.
  Starting with (p, q) from each pair (n, m) with n < m three 
  new pairs (n, 2n + m), (m, 2m - n), (m, 2m + n) are derived.
  This is done in depth first manner until (pred n m) is false 
  for a pair. This will then be ignored and not branch out to 
  new pairs. In case that p and q are not delivered as args, 
  two coprime-sequences (starting from (1, 2) and (1, 3) resp.) 
  are interleaved. This yields all coprime-pairs fulfilling the 
  given predicate. Does not cancel out starting arguments
  violating the predicate."
  ([pred] (util/interleave-all (coprimes 1 2 pred) (coprimes 1 3 pred)))
  ([p q pred]
    (let [children (fn [[n m]]
                     (->>
                       [[n (+ (* 2 n) m)]
                        [m (- (* 2 m) n)]
                        [m (+ (* 2 m) n)]]
                       (filter (fn [[n m]] (pred n m)))))
          walk (traversal/make-walk :depth :pre
                                    (constantly true) identity identity
                                    children)]
      (walk [p q]))))