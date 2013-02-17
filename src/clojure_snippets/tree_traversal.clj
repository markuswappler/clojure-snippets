(ns clojure-snippets.tree-traversal)

(defn queue [& items]
  (let [q clojure.lang.PersistentQueue/EMPTY]
    (if items 
      (apply conj q items)
      q)))

(defn post-cons [x seq]
  (lazy-seq
    (if (not-empty seq)
      (cons (first seq) (post-cons x (rest seq)))
      [x])))

(defn make-walk [strategy order branch? branch leaf children]
  (let [node-store (if (= :depth strategy) vector queue)
        node-cons (if (= :pre order) cons post-cons)]
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