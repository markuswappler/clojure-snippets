(ns clojure-snippets.util)

(defmacro condj 
  ([coll test item]
    `(if ~test
       (conj ~coll ~item)
       ~coll))
  ([coll test item & clauses]
    `(condj (condj ~coll ~test ~item) 
            ~(first clauses) 
            ~(second clauses) 
            ~@(nnext clauses))))

(defmacro cond-vector [& clauses]
  `(condj [] ~@clauses))

(defn make-matrix [rows]
  (let [rc (count rows)
        cc (count (rows 0))]
    (fn [kwd & [i j]]
      (condp = kwd
        :rows rc
        :cols cc
        :entry ((rows i) j)))))

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

(defn make-dijkstra [neighbors dist]
  (fn dijkstra [sources terminal?]
    (if (coll? terminal?)
      (dijkstra sources (fn [node] (some #{node} terminal?)))
      (loop [visited {}
             reachable (->> sources
                         (map (fn [s] {s {:dist 0 :prev nil}}))
                         (apply merge {}))]
        (when-let [[node node-data]
                   (first (sort-by (comp :dist val) reachable))]
          (if (terminal? node)
            (loop [path [{:node node :dist (node-data :dist)}] 
                   prev (node-data :prev)]
              (if-let [prev-data (visited prev)]
                (recur (conj path {:node prev :dist (prev-data :dist)})
                       (prev-data :prev))
                path))
            (let [updates (->> (neighbors node)
                            (filter (fn [nb] (not (visited nb))))
                            (map (fn [nb] [nb (+ (node-data :dist) (dist node nb))]))
                            (filter (fn [[nb d]]
                                      (if-let [r (reachable nb)]
                                        (> (r :dist) d)
                                        true)))
                            (map (fn [[nb d]] 
                                   {nb {:dist d 
                                        :prev node}})))]
              (recur (assoc visited node node-data)
                     (apply merge (dissoc reachable node) updates)))))))))