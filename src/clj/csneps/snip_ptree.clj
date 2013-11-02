(in-ns 'csneps.snip)

(defrecord PTree
  [tree
   pnode-ruiset-map]
  RUIStructure
  (get-rule-use-info [this new-rui]))

(defn merge-var-term
  [val-in-result val-in-latter]
  (concat val-in-result val-in-latter))

(defn term-vars
  "Returns the set of arbitrary terms which are in the down cableset of term."
  [term]
  (loop [dcs (:down-cableset term)
         vars #{}]
    (if (not (empty? dcs))
      (recur (rest dcs)
             (union vars (set (filter csneps/arbitraryTerm? (first dcs)))))
      vars)))

(defn var-pat-map
  [term]
  (apply clojure.core/merge (map (fn [var] {var [term]}) (term-vars term))))
  
(defn adjacent-pat-seq
  [pat-list pat-map]
  (loop [vars-unproc (keys pat-map)
         vars-dolist [(key (first pat-map))]
         vars-done #{}
         patseq []]
    (if (or (= (count patseq) (count pat-list)) (and (empty? vars-unproc) (empty? vars-dolist)))
      patseq
      (let [currvar (or (first vars-dolist) (first vars-unproc))
            varterms (get pat-map currvar)]
        (recur (difference (set vars-unproc) (set vars-dolist))
               (concat (rest vars-dolist) (filter #(not (some #{%} (conj vars-done currvar))) (apply union (map term-vars varterms))))
               (conj vars-done currvar)
               (concat patseq (filter #(not (some #{%} patseq)) varterms)))))))
    
(defn share-var?
  [term1 term2]
  (not (nil? (intersection (term-vars term1) (term-vars term2)))))

(defn pat-seq-to-ptree
  [patseq patlist]
  (loop [ptree '()
         patseq patseq]
    (if (empty? patseq)
      ptree
      (let [n (first patseq)
            nn (second patseq)]
        (if (and
              (not (empty? ptree))
              (or (nil? nn)
                  (not (share-var? n nn))))
          ;; Combine n with previous pattern.
          (recur
            (list ptree n)
            (rest patseq))
          ;; Add new pair of n, nn
          (recur
            (if (not (empty? ptree))
              (list ptree (list n nn))
              (list n nn))
            (rest (rest patseq))))))))

(defn walk-list
  [inner outer form]
  (if (list? form) 
    (outer (apply list (map inner form)))
    (outer form)))

(defn postwalk-list 
  [f form]
  (walk-list (partial postwalk-list f) f form))

(defn make-pnode-ruiset-map
  [ptree]
  (let [m (ref {})]
    (dosync 
      (postwalk-list (fn [x] (alter m assoc x (ref #{})) x) ptree))
    @m))

(defn make-ptree
  [syntype fillers]
  (let [ants (condp = syntype
               :csneps.core/Conjunction (first fillers)
               :csneps.core/Numericalentailment (first fillers));; Only and-entailment!
        var-pat-map (apply merge-with merge-var-term (map var-pat-map ants))
        adj-pat-seq (adjacent-pat-seq ants var-pat-map)
        ptree (pat-seq-to-ptree adj-pat-seq ants)]
    (PTree. ptree (make-pnode-ruiset-map ptree))))

(defn test-ptree
  []
  (dosync (ref-set build/KRNovice true))
  (let [a (build/assert '(and (A (every x (Isa x v1)) (every y (Isa y v2)))
                              (B (every z (Isa z v3)) (every w (Isa w v4)))
                              (C z (every v (Isa v v5)))
                              (D x z)
                              (E y v)
                              (G y z)
                              (H x w))
                        (ct/currentContext) :hyp)]
    (make-ptree :csneps.core/Conjunction (:down-cableset a))))
  