(ns clojure-snippets.euler
  (:require [clojure.math.numeric-tower :as numeric]
            [clojure-snippets.math :as math]))

;; helper

(defn int-exp 
  "integer number via exponential notation"
  [m e]  
  (* m (numeric/expt 10 e)))

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
;; fk = 4 * fk-3 + fk-6 

(defn solve-2 []
  (let [bound (int-exp 4 6)]
    (->> 
      ((math/make-fib 1 4) 2 8)
      (take-while (fn [a] (>= bound a)))
      (reduce +))))

;; 15
;; make 40 decisions: right or down
;; choose 20 down-decisions from all decisions

(defn solve-15 []
  (math/binom 40 20))

;; 351
;; TODO

(defn solve-351 []
  (let [bound (int-exp 10 8)]
    (count (math/coprimes (fn [n m] (>= bound (+ n m)))))))