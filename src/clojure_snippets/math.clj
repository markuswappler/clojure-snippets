(ns clojure-snippets.math
  (:require [clojure.math.numeric-tower :as numeric] 
            [clojure-snippets.util :as util]))

(defn int-exp 
  "integer number via exponential notation"
  [m e]  
  (* m (numeric/expt 10 e)))

(defn sgn [x]
  (cond 
    (neg? x) -1
    (pos? x) 1
    :else 0))

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
  "Fast computation of (reduce + (range ...)).
  Contrary to the range function both bounds are inclusive."
  ([n] (quot (* n (inc n)) 2))
  ([n0 n1] (range-sum n0 n1 1))
  ([n0 n1 step]
    (cond
      (zero? step) 0
      (neg? step) (range-sum n1 n0 (- step))
      (> n0 n1) 0
      :else (let [shift (- step n0)
                  n (quot (+ n1 shift) step)]
              (- (* step (range-sum n)) 
                 (* n shift))))))
  
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
          sqrt+1 (inc (numeric/floor (numeric/sqrt n)))
          ps (boolean-array n+1 true)]
      (doseq [even (range 4 n+1 2)]
        (aset-boolean ps even false))
      (doseq [k (range 3 sqrt+1)]
        (if (aget ps k)
          (doseq [mult (range (* k k) n+1 (* 2 k))]
            (aset-boolean ps mult false))))
      (cons 2 (for [k (range 3 n+1 2)
                    :when (aget ps k)]
                k)))))

;; Idea from http://mathoverflow.net/questions/99473/calculating-mobius-function
;; Last mutation (> 0 val) -> 1, (< 0 val) -> -1 is necessary because
;; of the sqrt-bound of the primes. First condition is not fulfilled if
;; k contains a prime greater than the sqrt-bound. But this can only happen
;; once, thus just flip the sign.
(defn moebius 
  "Generates array of the values of the möbius function from 1 to n.
  Ignore value at position 0."
  [n]  
  (if (< 0 n)
    (let [n+1 (inc n)
          ps (primes (numeric/floor (numeric/sqrt n)))
          mus (int-array n+1 1)]
      (doseq [sqr (map #(* % %) ps)
              mult (range sqr n+1 sqr)]
        (aset-int mus mult 0))
      (doseq [p ps
              mult (range p n+1 p)
              :let [val (aget mus mult)]]
        (aset-int mus mult (* val (- p))))
      (doseq [k (range 2 n+1)
              :let [val (aget mus k)]]
        (aset-int mus k (cond
                          (= k (numeric/abs val)) (sgn val)
                          (neg? val) 1
                          (pos? val) -1
                          :else 0)))
      mus)))

;; http://mathworld.wolfram.com/TotientSummatoryFunction.html
(defn phi-summatory 
  "Sum of Euler's phi function phi(k) 
  for k from 1 to n or n0 to n1, resp."
  ([n] (phi-summatory 1 n))
  ([n0 n1]
    (let [mus (rest (seq (moebius n1)))
          ps (fn [n]
               (let [facs (for [d (range 1 (inc n))]
                            (range-sum (quot n d)))]
                 (reduce + (map * mus facs))))]
      (if (< 1 n0)
        (- (ps n1) (ps (dec n0)))
        (ps n1)))))

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
          walk (util/make-treewalk :depth :pre
                                   (constantly true) identity identity
                                   children)]
      (walk [p q]))))