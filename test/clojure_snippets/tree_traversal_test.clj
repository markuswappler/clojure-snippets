(ns clojure-snippets.tree-traversal-test
  (:use clojure.test
        clojure-snippets.tree-traversal))

(deftest test-queue
  (let [q0 (queue)
        q1 (conj q0 1 2 3)
        q2 (queue 1 2 3)]
    (is (empty? q0))
    (is (= 1 (peek q1)))
    (is (= 1 (peek q2)))
    (is (= 2 (peek (pop q1))))
    (is (= 2 (peek (pop q2))))
    (is (= 3 (peek (pop (pop q1)))))
    (is (= 3 (peek (pop (pop q2)))))
    (is (empty? (pop (pop (pop q1)))))
    (is (empty? (pop (pop (pop q2)))))))

(deftest test-post-cons
  (is (= '(1 2 3 4) (post-cons 4 [1 2 3])))
  (is (= '(1 2 3 4) (post-cons 4 '(1 2 3)))))

(deftest test-make-walk
  (testing "binary tree"
           (is (= '([5 6] 6 5 [[1 2] [3 4]] [3 4] 4 3 [1 2] 2 1)
                  ((make-walk :depth :pre vector? identity identity identity)
                    [[1 2] [3 4]] [5 6])))
           (is (= '([[1 2] [3 4]] [1 2] 1 2 [3 4] 3 4 [5 6] 5 6)
                  ((make-walk :depth :pre vector? identity identity reverse)
                    [5 6] [[1 2] [3 4]])))
           (is (= '(1 2 [1 2] 3 4 [3 4] [[1 2] [3 4]] 5 6 [5 6])
                  ((make-walk :depth :post vector? identity identity identity)
                    [[1 2] [3 4]] [5 6])))
           (is (= '([[1 2] [3 4]] [5 6] [1 2] [3 4] 5 6 1 2 3 4)
                  ((make-walk :breadth :pre vector? identity identity identity)
                    [[1 2] [3 4]] [5 6])))
           (is (= '(4 3 2 1 6 5 [3 4] [1 2] [5 6] [[1 2] [3 4]])
                  ((make-walk :breadth :post vector? identity identity identity)
                    [[1 2] [3 4]] [5 6]))))
  (testing "range"
           (let [walk (make-walk :depth :pre (constantly true) 
                                 identity nil (comp vector inc))]
             (is (= (range 1 11) 
                    (take 10 (walk 1))))
             (is (= (range 42 4711) 
                    (take (- 4711 42) (walk 42))))))
  (testing "fibonacci"
           (let [walk (make-walk :depth :pre (constantly true)
                                 first nil (fn [[a b]] [[b (+ a b)]]))]
             (is (= '(0 1 1 2 3 5 8 13 21 34 55 89) 
                    (take 12 (walk [0 1]))))
             (is (= '(2 6 8 14 22 36 58 94)
                    (take 8 (walk [2 6]))))))) 