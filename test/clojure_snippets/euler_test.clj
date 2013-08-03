(ns clojure-snippets.euler-test
  (:require [clojure-snippets.util :as util])
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

(deftest test-solve-4
  (is (= 9009 (solve-4 2))))

(deftest test-solve-5
  (is (= 2520 (solve-5 10))))

(deftest test-solve-6
  (is (= 2640 (solve-6 10))))

(deftest test-solve-7
  (is (= '(2 3 5 7 11 13) (map solve-7 (range 1 7)))))

(deftest test-solve-8
  (is (= 64 (solve-8 [1 2 2 2 2 4]))))

(deftest test-solve-9
  (is (= (* 3 4 5) (solve-9 (+ 3 4 5)))))

(deftest test-solve-10
  (is (= (+ 2 3 5 7) (solve-10 10))))

(deftest test-solve-11
  (is (= 16 (solve-11 (util/make-matrix [[2 0 0 1]
                                         [1 2 1 1]
                                         [0 1 2 1]
                                         [0 0 1 2]])))))

(deftest test-solve-12
  (is (= 3 (solve-12 1)))
  (is (= 6 (solve-12 2)))
  (is (= 6 (solve-12 3)))
  (is (= 28 (solve-12 4)))
  (is (= 28 (solve-12 5))))

(deftest test-solve-15
  (is (= 6 (solve-15 2))))

(deftest test-solve-18
  (is (= 23 (solve-18 (util/make-matrix [[3] 
                                         [7 4] 
                                         [2 4 6] 
                                         [8 5 9 3]])))))

(deftest test-solve-20
  (is (= 27 (solve-20 10))))

(deftest test-solve-81
  (is (= 2427 (solve-81 (util/make-matrix [[131	673	234	103	18]
                                           [201	96	342	965	150]
                                           [630	803	746	422	111]
                                           [537	699	497	121	956]
                                           [805	732	524	37	331]])))))

(deftest test-solve-82
  (is (= 994 (solve-82 (util/make-matrix [[131	673	234	103	18]
                                          [201	96	342	965	150]
                                          [630	803	746	422	111]
                                          [537	699	497	121	956]
                                          [805	732	524	37	331]])))))

(deftest test-solve-83
  (is (= 2297 (solve-83 (util/make-matrix [[131	673	234	103	18]
                                           [201	96	342	965	150]
                                           [630	803	746	422	111]
                                           [537	699	497	121	956]
                                           [805	732	524	37	331]])))))

(deftest test-solve-107
  (is (= 150 (solve-107 (util/make-matrix [[nil	16 12 21 nil nil nil]
                                           [16 nil nil 17 20 nil nil]
                                           [12 nil nil 28 nil 31 nil]
                                           [21 17 28 nil 18 19 23]
                                           [nil 20 nil 18 nil nil 11]
                                           [nil nil 31 19 nil nil 27]
                                           [nil nil nil 23 11 27 nil]])))))

(deftest test-solve-351-slow
  (is (= 30 (solve-351-slow 5)))
  (is (= 138 (solve-351-slow 10)))
  (is (= 1177848 (solve-351-slow 1000))))

(deftest test-solve-351
  (is (= 30 (solve-351 5)))
  (is (= 138 (solve-351 10)))
  (is (= 1177848 (solve-351 1000))))