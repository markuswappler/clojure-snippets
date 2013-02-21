(ns clojure-snippets.tree-traversal
  (:require [clojure-snippets.util :as util]))

(defn make-walk [strategy order branch? branch leaf children]
  (let [node-store (if (= :depth strategy) vector util/queue)
        node-cons (if (= :pre order) cons util/post-cons)]
    (fn [& nodes]
      (let [nodes (apply node-store nodes)
            walk (fn walk [nodes]
                   (lazy-seq
                     (when-let [node (peek nodes)]
                       (let [[item children]
                             (if (branch? node)
                               [(branch node) (children node)]
                               [(leaf node) []])]
                         (node-cons
                           item
                           (if (empty? children)
                             (walk (pop nodes))
                             (walk (apply conj (pop nodes) children))))))))]
        (walk nodes)))))