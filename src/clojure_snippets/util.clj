(ns clojure-snippets.util)

(defmacro condj
  "cond(itional )join
  Like conj with the difference that each item
  has a prepending test. The item is conjoined
  only in the case of test being evaluated to 
  logical true."
  ([coll test item]
    `(if ~test
       (conj ~coll ~item)
       ~coll))
  ([coll test item & clauses]
    `(condj (condj ~coll ~test ~item) 
            ~(first clauses) 
            ~(second clauses) 
            ~@(nnext clauses))))

(defmacro cond-vector 
  "conditional vector
  Creates a vector in the sense of condj."
  [& clauses]
  `(condj [] ~@clauses))

(defn make-matrix [rows]
  (let [rc (count rows)
        cc (count (nth rows 0))]
    (fn [kwd & [i j]]
      (condp = kwd
        :rows rc
        :cols cc
        :entry (nth (nth rows i) j)))))

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

(defn make-treewalk 
  "Makes a tree traversal function.
  Arguments:
    - strategy :depth for depth first search
               :breadth for breadth first search
    - order :pre for pre order traversal
            :post for post order traversal
    - branch? a predicate whether a node branches
    - branch function applied on branch nodes
    - leaf function applied on leaf nodes
    - children determines the collection of
               children of a branch node
  Result: 
    A function that takes start nodes and
    produces a lazy sequence of the
    traversed nodes manipulated by
    the branch or leaf function."
  [strategy order branch? branch leaf children]
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

(defn make-dijkstra 
  "Implementation of Dijkstra's shortest path algorithm.
  It takes two functions: 
    - neighbors determines for a given node 
      a collection of all neighbors of this node.
    - dist determines for two given nodes
      the distance between these nodes.
  Output is a function that executes Dijkstra's algorithm
  on the weighted graph specified through neighbors and dist.
  It has two arguments:
    - sources is a collection of source nodes 
      where the path has to start.
    - terminal? specifies where the path has
      to end. Either by a collection of
      terminal nodes or by a predicate function
      that determines for a node whether this
      node is a terminal node.
  Output is the path in reverse order. It is a vector of
  maps of the form {:node v :dist k} where v is a node
  and k the distance of v to the respective source node."  
  [neighbors dist]
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

(defn kruskal 
  "Executes Kruskal's algorithm to determine a minimum spanning forest 
  of a graph. If the graph is connected the forest is a tree.
  Input: A collection or set of (nonnegatively) 
         weighted edges {v1,v2,w>=0} of the form 
         {:node-1 v1 :node-2 v2 :weight w}.
  Output: A vector of the edges belonging to the forest 
          in ascending order according to the weights.
  Note that the nodes of the graph are assumed to be the set of 
  all nodes being incident with one of the given edges. 
  Additional isolated nodes do not matter."
  [edges]
  (loop [edges (sort-by :weight edges)
         components #{}
         forest []]
    (if-let [{:keys [node-1 node-2 weight] :as edge} (first edges)]
      (let [find-comp (fn [node]
                        (if-let [comp (first (filter #(% node) components))]
                          comp
                          #{node}))
            comp-1 (find-comp node-1)
            comp-2 (find-comp node-2)]
        (if (= comp-1 comp-2)
          (recur (rest edges) 
                 components 
                 forest)
          (recur (rest edges)
                 (-> components
                   (disj comp-1 comp-2)
                   (conj (clojure.set/union comp-1 comp-2)))
                 (conj forest edge))))
      forest)))    