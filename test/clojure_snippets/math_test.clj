(ns clojure-snippets.math-test
  (:require clojure.set)
  (:use clojure.test
        clojure-snippets.math))

(deftest sgn-test
  (is (= 0 (sgn 0)))
  (is (= 0 (sgn 0.0)))
  (is (= -1 (sgn -42)))
  (is (= 1 (sgn 42)))
  (is (= -1 (sgn -47.11)))
  (is (= 1 (sgn 47.11))))

(deftest binom-test
  (let [pascal-row (fn [n] (for [k (range (inc n))] (binom n k)))]
    (is (= '(1) (pascal-row 0)))
    (is (= '(1 1) (pascal-row 1)))
    (is (= '(1 2 1) (pascal-row 2)))
    (is (= '(1 3 3 1) (pascal-row 3)))
    (is (= '(1 4 6 4 1) (pascal-row 4)))
    (is (= '(1 5 10 10 5 1) (pascal-row 5)))
    (is (= '(1 6 15 20 15 6 1) (pascal-row 6)))
    (is (= '(1 7 21 35 35 21 7 1) (pascal-row 7)))))

(deftest range-sum-test
  (is (= (+ 1 2 3 4 5) (range-sum 5)))
  (is (= (+ 41 42) (range-sum 41 42)))
  (is (zero? (range-sum 42 41)))
  (is (= 42 (range-sum 42 42)))
  (is (= (+ 5 8 11) (range-sum 5 11 3)))
  (is (= (+ 5 8 11) (range-sum 5 12 3)))
  (is (= (+ 5 8 11) (range-sum 5 13 3)))
  (is (= (+ 5 8 11 14) (range-sum 5 14 3)))
  (is (= (+ 1 3 5 9)) (range-sum 1 10 2))
  (is (= (+ -5 -1 3 7) (range-sum -5 7 4)))
  (is (= (+ 6 4 2 0 -2) (range-sum 6 -2 -2)))
  (is (= (+ 1 2 3 4 5) (range-sum 5 1 -1)))
  (is (zero? (range-sum 1 10 0)))
  (is (zero? (range-sum 1 10 -1)))
  (is (zero? (range-sum 10 1 1)))
  (is (= 4711 (range-sum 4711 4711 1))))  

(deftest make-fib-test
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)
         (take 16 ((make-fib 1 1) 0 1))))
  (is (= '(1 2 1 2 1 2 1 2)
         (take 8 ((make-fib 1 0) 1 2))))
  (is (= '(1 4 14 50)
         (take 4 ((make-fib 2 3) 1 4))))
  (is (= '(-1 1 2 1 -1 -2 -1 1 2 1 -1 -2 -1 1 2 1)
         (take 16 ((make-fib -1 1) -1 1)))))

(deftest primes-test
  (let [ps '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 
             47 53 59 61 67 71 73 79 83 89 97 101)]
    (is (= ps (primes 101)))
    (is (= ps (primes 102)))))

(deftest moebius-test
  (let [mus [1 -1 -1 0 -1 1 -1 0 0 1 -1 0 -1 1 1 0 -1 0 -1 0 1 1 -1 0 0]]
    (is (= mus (rest (into [] (moebius 25)))))))

(deftest phi-summatory-test
  (let [sums [1 2 4 6 10 12 18 22 28]]
    (is (= sums (map phi-summatory (range 1 10))))
    (is (= (- (phi-summatory 4711) (phi-summatory 41))
           (phi-summatory 42 4711)))
    (is (= (phi-summatory 100) (phi-summatory 1 100)))))
        
(deftest coprimes-test
  (let [odd #{[1 2] [1 4] [1 6] [1 8]
              [2 3] [2 5] [2 7] [2 9]
              [3 4] [3 8]
              [4 5] [4 7] [4 9]
              [5 6] [5 8]
              [6 7]
              [7 8]
              [8 9]}
        even #{[1 3] [1 5] [1 7] [1 9]
               [3 5] [3 7]
               [5 7] [5 9]
               [7 9]}
        pred (fn [_ m] (> 10 m))]                   
    (is (= odd (set (coprimes 1 2 pred))))
    (is (= even (set (coprimes 1 3 pred))))
    (is (= (clojure.set/union odd even) (set (coprimes pred))))))