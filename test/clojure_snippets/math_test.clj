(ns clojure-snippets.math-test
  (:require clojure.set)
  (:use clojure.test
        clojure-snippets.math))

(deftest test-sgn
  (is (= 0 (sgn 0)))
  (is (= 0 (sgn 0.0)))
  (is (= -1 (sgn -42)))
  (is (= 1 (sgn 42)))
  (is (= -1 (sgn -47.11)))
  (is (= 1 (sgn 47.11))))

(deftest test-expt
  (testing "exponentiation"
           (let [pow (partial expt * 1)]
             (is (= 1 (pow 0 0)))
             (is (= 0 (pow 0 1)))
             (is (= 0 (pow 0 2)))
             (is (= 1 (pow 2 0)))
             (is (= 2 (pow 2 1)))
             (is (= 4 (pow 2 2)))
             (is (= 128 (pow 2 7)))
             (is (= 256 (pow 2 8)))
             (is (= 512 (pow 2 9)))
             (is (= 1024 (pow 2 10)))
             (is (= 27 (pow 3 3)))
             (is (= 81 (pow 3 4)))
             (is (= 64 (pow 4 3)))))
  (testing "multiplication"
           (let [mult (partial expt + 0)]
             (is (= 0 (mult 3 0)))
             (is (= 0 (mult 0 3)))
             (is (= 12 (mult 3 4)))
             (is (= 12 (mult 4 3))))))

(deftest test-binom
  (let [pascal-row (fn [n] (for [k (range (inc n))] (binom n k)))]
    (is (= '(1) (pascal-row 0)))
    (is (= '(1 1) (pascal-row 1)))
    (is (= '(1 2 1) (pascal-row 2)))
    (is (= '(1 3 3 1) (pascal-row 3)))
    (is (= '(1 4 6 4 1) (pascal-row 4)))
    (is (= '(1 5 10 10 5 1) (pascal-row 5)))
    (is (= '(1 6 15 20 15 6 1) (pascal-row 6)))
    (is (= '(1 7 21 35 35 21 7 1) (pascal-row 7)))))

(deftest test-plus
  (let [p (partial +mod 4)]
    (is (= 0 (p 0 0)))
    (is (= 1 (p 0 1)))
    (is (= 1 (p 1 0)))
    (is (= 2 (p 0 2)))
    (is (= 2 (p 1 1)))
    (is (= 2 (p 2 0)))
    (is (= 3 (p 0 3)))
    (is (= 3 (p 1 2)))
    (is (= 3 (p 2 1)))
    (is (= 3 (p 3 0)))
    (is (= 0 (p 1 3)))
    (is (= 0 (p 2 2)))
    (is (= 0 (p 3 1)))
    (is (= 1 (p 2 3)))
    (is (= 1 (p 3 2)))
    (is (= 2 (p 3 3)))
    (is (= 7 (+mod 12 3 4)))
    (is (= 7 (+mod 12 6 13)))
    (is (= 7 (+mod 12 10 9)))
    (is (= 4 (+mod 12 8 8)))))    

(deftest test-range-sum
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

(deftest test-make-fib
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)
         (take 16 ((make-fib 1 1) 0 1))))
  (is (= '(1 2 1 2 1 2 1 2)
         (take 8 ((make-fib 1 0) 1 2))))
  (is (= '(1 4 14 50)
         (take 4 ((make-fib 2 3) 1 4))))
  (is (= '(-1 1 2 1 -1 -2 -1 1 2 1 -1 -2 -1 1 2 1)
         (take 16 ((make-fib -1 1) -1 1)))))

(deftest test-fib
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610)
         (map fib (range 16)))))

(deftest test-collatz
  (is (= '(3 10 5 16 8 4 2 1) (collatz 3)))
  (is (= '(13 40 20 10 5 16 8 4 2 1) (collatz 13)))
  (is (= '(14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1) (collatz 14))))

(deftest test-primes
  (let [ps '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 
             47 53 59 61 67 71 73 79 83 89 97 101)]
    (is (= ps (primes 101)))
    (is (= ps (primes 102)))))

(deftest test-divisors
  (is (= '() (divisors 0)))
  (is (= '(1) (divisors 1)))
  (is (= '(1 2) (divisors 2)))
  (is (= '(1 3) (divisors 3)))
  (is (= '(1 2 4) (divisors 4)))
  (is (= '(1 5) (divisors 5)))
  (is (= '(1 2 3 6) (divisors 6)))
  (is (= '(1 7) (divisors 7)))
  (is (= '(1 2 4 8) (divisors 8))))

(deftest test-tau
  (is (= 0 (tau 0)))
  (is (= 1 (tau 1)))
  (is (= 2 (tau 2)))
  (is (= 2 (tau 3)))
  (is (= 3 (tau 4)))
  (is (= 2 (tau 5)))
  (is (= 4 (tau 6)))
  (is (= 2 (tau 7)))
  (is (= 4 (tau 8))))

(deftest test-moebius
  (let [mus [1 -1 -1 0 -1 1 -1 0 0 1 -1 0 -1 1 1 0 -1 0 -1 0 1 1 -1 0 0]]
    (is (= mus (rest (into [] (moebius 25)))))))

(deftest test-phi-summatory
  (let [sums [1 2 4 6 10 12 18 22 28]]
    (is (= sums (map phi-summatory (range 1 10))))
    (is (= (- (phi-summatory 4711) (phi-summatory 41))
           (phi-summatory 42 4711)))
    (is (= (phi-summatory 100) (phi-summatory 1 100)))))
        
(deftest test-coprimes
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