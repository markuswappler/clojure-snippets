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
  (is false))