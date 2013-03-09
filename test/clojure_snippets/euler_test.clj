(ns clojure-snippets.euler-test
  (:use clojure.test
        clojure-snippets.euler))

(deftest test-solve-1
  (is (= 23 (solve-1 10))))

(deftest test-solve-1-sample
  (is (= 23 (solve-1-sample 10))))

(deftest test-solve-2
  (is (= (+ 2 8 34) (solve-2 89))))

(deftest test-solve-3
  (is (= 29 (solve-3 13195))))

(deftest test-solve 4
  (is (= 9009 (solve-4 2))))

(deftest test-solve-5
  (is (= 2520 (solve-5 10))))

(deftest test-solve-6
  (is (= 2640 (solve-6 10))))

(deftest test-solve-7
  (is (= '(2 3 5 7 11 13) (map solve-7 (range 1 7)))))

(deftest test-solve-8
  (is (= 64 (solve-8 "122224"))))

(deftest test-solve-9
  (is (= (* 3 4 5) (solve-9 (+ 3 4 5)))))

(deftest test-solve-10
  (is (= (+ 2 3 5 7) (solve-10 10))))

(deftest test-solve-15
  (is (= 6 (solve-15 2))))

(deftest test-solve-351-slow
  (is (= 30 (solve-351-slow 5)))
  (is (= 138 (solve-351-slow 10)))
  (is (= 1177848 (solve-351-slow 1000))))

(deftest test-solve-351
  (is (= 30 (solve-351 5)))
  (is (= 138 (solve-351 10)))
  (is (= 1177848 (solve-351 1000))))