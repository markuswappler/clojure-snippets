(ns clojure-snippets.tree-traversal)

(defn walk-fn [strategy order branch? branch leaf children]
  (let [revcat #(concat %2 %1)
        node-cat (if (= :depth strategy) concat revcat)
        res-cat (if (= :pre order) concat revcat)]
    (fn walk [& nodes]
      (lazy-seq
        (when-let [[node & nodes] nodes]      
          (if (branch? node)
            (res-cat [(branch node)] (apply walk (node-cat (children node) nodes)))
            (res-cat [(leaf node)] (apply walk nodes))))))))