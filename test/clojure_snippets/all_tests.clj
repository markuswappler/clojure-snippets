(ns clojure-snippets.all-tests
  (:use clojure.test
        clojure-snippets.euler-test
        clojure-snippets.expressions-test
        clojure-snippets.mastermind-test
        clojure-snippets.queens-test
        clojure-snippets.tree-traversal-test
        clojure-snippets.util-test))

(defn run []
  (run-all-tests #"clojure-snippets[\w\.\-]*test"))