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

(defn make-fib 
  "Returns a function that takes two arguments [a0 a1] and
  returns a lazy (generalized) Fibonacci sequence with
  f0 = a0, f1 = a1, fk = c1 * fk-1 + c0 * fk-2."
  [c0 c1]
  (fn fib [a0 a1] 
    (lazy-seq 
      (cons a0 (fib a1 (+ (* c0 a0) (* c1 a1)))))))

(defn primes
  "Lists all prime numbers up to n via the sieve of Eratosthenes."
  [n]
  (let [cancel-out (fn [forbidden numbers]
                     (loop [forbidden forbidden numbers numbers sieved []]
                       (if (or (empty? forbidden) (empty? numbers))
                         (concat sieved numbers)
                         (let [p (first forbidden)
                               k (first numbers)]
                           (cond
                             (< p k) (recur (rest forbidden) numbers sieved)
                             (= p k) (recur (rest forbidden) (rest numbers) sieved)
                             (> p k) (recur forbidden (rest numbers) (conj sieved k)))))))]
    (loop [numbers (range 3 (inc n) 2) primes []]
      (if-let [p (first numbers)]
        (let [multiples (cons p (range (* p p) (inc n) (* 2 p)))]
          (recur (cancel-out multiples numbers) (conj primes p)))
        (cons 2 primes)))))

(defn coprimes
  "Generates a lazy sequence of pairs of coprime integers.
  Starting with (p, q) from each pair (n, m) with n < m three 
  new pairs (n, 2n + m), (m, 2m - n), (m, 2m + n) are derived.
  This is done in depth first manner until (pred n m) is false 
  for a pair. This will then be ignored and not branch out to 
  new pairs. In case that p and q are not delivered as args, 
  two coprime-sequences (starting from (1, 2) and (1, 3) resp.) 
  are interleaved. This yields all coprime-pairs fulfilling the 
  given predicate."
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