(ns clojure-snippets.all-tests
  (:use clojure.test
        clojure-snippets.euler-test
        clojure-snippets.expressions-test
        clojure-snippets.mastermind-test
        clojure-snippets.math-test
        clojure-snippets.queens-test
        clojure-snippets.util-test
        clojure-snippets.date-test))

(defn run []
  (run-all-tests #"clojure-snippets[\w\.\-]*test"))