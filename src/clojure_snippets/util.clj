(ns clojure-snippets.util)

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