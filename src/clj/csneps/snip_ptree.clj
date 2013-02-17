(in-ns 'csneps.snip)

(defrecord PTree
  [tree]
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
    
(defn pat-seq-to-ptree
  [patseq patlist]
  )

(defn make-ptree
  [syntype fillers]
  (let [ants (condp = syntype
               :csneps.core/Conjunction (first fillers)
               :csneps.core/Numericalentailment (first fillers));; Only and-entailment!
        var-pat-map (apply merge-with merge-var-term (map var-pat-map ants))
        adj-pat-seq (adjacent-pat-seq ants var-pat-map)
        ptree (pat-seq-to-ptree adj-pat-seq ants)]
    ptree))