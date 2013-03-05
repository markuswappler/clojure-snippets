(ns clojure-snippets.util)

;; Idea from 
;; http://www.learningclojure.com/2010/09/clojure-macro-tutorial-part-i-getting.html
(defmacro dbg [x]
  `(let [x# ~x]
     (println "dbg:" '~x "->" x#)
     x#))

(defn interleave-all [c1 c2]
  (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (cond
        (and s1 s2) (cons (first s1) 
                          (cons (first s2)
                                (interleave-all (rest s1) (rest s2))))
        s1 s1
        s2 s2))))

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

(defn make-treewalk [strategy order branch? branch leaf children]
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