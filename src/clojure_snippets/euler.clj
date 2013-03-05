(ns clojure-snippets.euler
  (:require [clojure.math.numeric-tower :as numeric]
            [clojure-snippets.math :as math]
            [clojure-snippets.util :as util]))

;; 1

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

;; 2
;; Every third Fibonacci number is even: 2, 8, 34, 144, ...
;; fk = 4 * fk-3 + fk-6 

(defn solve-2 
  ([] (solve-2 (math/int-exp 4 6)))
  ([n]
    (->> 
      ((math/make-fib 1 4) 2 8)
      (take-while (fn [a] (>= n a)))
      (reduce +))))

;; 7
;; The nth prime is about n * log(n).
;; Thus, take 2n * (log(n) + 1) as upper bound

(defn solve-7
  ([] (solve-7 (inc (math/int-exp 1 4))))
  ([n]
    (nth (math/primes (* 2 n (inc (numeric/ceil (Math/log n))))) 
         (dec n))))

;; 10

(defn solve-10
  ([] (solve-10 (math/int-exp 2 6)))
  ([n] (reduce + (math/primes (dec n)))))

;; 15
;; make 2n decisions: right or down
;; choose n down-decisions from all decisions

(defn solve-15 
  ([] (solve-15 20))
  ([n] (math/binom (* 2 n) n)))

;; 351

;; http://en.wikipedia.org/wiki/Coprimes
(defn solve-351-slow
  ([] (solve-351-slow (math/int-exp 1 8)))
  ([n]
    (let [covered (fn [p q] (dec (quot n (+ p q))))
          covered-all (fn [p q]
                        (->> (math/coprimes p q #(< 0 (covered %1 %2)))
                          (map (partial apply covered))
                          (reduce +)))
          covered-12 (future (covered-all 1 2))
          covered-13 (future (covered-all 1 3))]
      (+ (* 6 (covered 0 1))
         (* 6 (covered 1 1))
         (* 12 @covered-12)
         (* 12 @covered-13)))))

;; http://mathworld.wolfram.com/TotientSummatoryFunction.html
(defn solve-351 
  ([] (solve-351 (math/int-exp 1 8)))
  ([n]
    (let [total (inc (* 6 (math/range-sum n))) ;; arithmetic series (in "radius")
          phi-sum-3-to-n (- (math/phi-summatory n) 2)
          lighted (+ 13
                     (* 6 phi-sum-3-to-n))]
      (- total lighted))))