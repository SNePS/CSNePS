(in-ns 'csneps.snip)

(declare msgs-to-promote)

(defrecord PTree
  [tree
   term-to-pnode-map]
  MessageStructure
  (get-rule-use-info [this new-msg]
    (let [starting-pnode ((:term-to-pnode-map this) (ffirst (:flaggedns new-msg)))
          starting-msgset (:msgset starting-pnode)]
      (if (@starting-msgset new-msg)
        ;; If we've already seen the message, stop now.
        #{}
        ;; Otherwise, lets see how far this takes us!
        (loop [currnode starting-pnode
               totest #{new-msg}]
          ;; Start by adding totest to the current node.
          ;; TODO: Do we need to filter these to check for existing? 
          (dosync (alter (:msgset currnode) union totest))
          (if (nil? @(:parent currnode))
            totest
            (let [promote (msgs-to-promote totest currnode)]
              (if (empty? promote)
                #{}
                (recur
                  @(:parent currnode)
                  promote)))))))))

(defrecord PNode
  [parent
   data
   left
   right
   msgset])

(defn sibling 
  [pnode]
  (let [parent @(:parent pnode)
        sibling (when parent
                  (if (= pnode (:left parent))
                    (:right parent)
                    (:left parent)))]
    sibling))

(defn- promote-msg-helper
  [new-msg sibling-msgs]
  (let [compat-msgs (filter #(compatible? % new-msg) sibling-msgs)
        merged-msgs (set (map #(merge new-msg %) compat-msgs))]
    merged-msgs))

(defn msgs-to-promote
  "Messages which should be promoted to the next level. Found by collecting 
    successful combinations with sibling messages."
  [msgs pnode]
  (let [sibling-pnode (sibling pnode)
        sibling-msgs (when sibling-pnode @(:msgset sibling-pnode))]
    (if (empty? sibling-msgs)
      #{}
      (apply union (map #(promote-msg-helper % sibling-msgs) msgs)))))

(defn merge-var-term
  [val-in-result val-in-latter]
  (concat val-in-result val-in-latter))

(defn term-vars
  "Returns the set of arbitrary terms which are in the down cableset of term."
  [term]
  (loop [dcs (@down-cableset term)
         vars #{}]
    (if (seq dcs)
      (recur (rest dcs)
             (union vars (set (filter arbitraryTerm? (first dcs)))))
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
  (loop [root nil
         patseq patseq
         term-to-pnode-map {}]
    (binding [*print-level* 2] (println root))
    (if (empty? patseq)
      [root term-to-pnode-map]
      (let [n (first patseq)
            nn (second patseq)]
        (if (and
              (not (nil? root))
              (or (nil? nn)
                  (not (share-var? n nn))))
          ;; Combine n with previous pattern.
          (let [newchild (PNode. (ref nil) n nil nil (ref #{}))
                newroot (PNode. (ref nil) (list (:data root) n) root newchild (ref #{}))]
            (dosync (ref-set (:parent newchild) newroot))
            (recur
              newroot
              (rest patseq)
              (assoc term-to-pnode-map n newchild)))
          ;; Add new pair of n, nn
          (if (not (nil? root))
            ;; New root node, one child is the node with the children n and nn.
            (let [newleft (PNode. (ref nil) n nil nil (ref #{}))
                  newright (PNode. (ref nil) nn nil nil (ref #{}))
                  newchild (PNode. (ref nil) (list n nn) newleft newright (ref #{}))
                  newroot (PNode. (ref nil) (list (:data root) (list n nn)) root newchild (ref #{}))]
              (dosync 
                (ref-set (:parent newchild) newroot)
                (ref-set (:parent newleft) newchild)
                (ref-set (:parent newright) newchild))
              (recur
                newroot
                (rest (rest patseq))
                (assoc term-to-pnode-map n newleft nn newright)))
            ;; Root is nil, so this is the root.
            (let [newleft (PNode. (ref nil) n nil nil (ref #{}))
                  newright (PNode. (ref nil) nn nil nil (ref #{}))
                  newroot (PNode. (ref nil) (list n nn) newleft newright (ref #{}))]
              (dosync
                (ref-set (:parent newleft) newroot)
                (ref-set (:parent newright) newroot))
              (recur
                newroot
                (rest (rest patseq))
                (assoc term-to-pnode-map n newleft nn newright)))))))))

(defn make-ptree
  [syntype fillers]
  (let [ants (condp = syntype
               :csneps.core/Conjunction (first fillers)
               :csneps.core/Numericalentailment (first fillers));; Only and-entailment!
        var-pat-map (apply merge-with merge-var-term (map var-pat-map ants))
        adj-pat-seq (adjacent-pat-seq ants var-pat-map)
        [ptree tpmap] (pat-seq-to-ptree adj-pat-seq ants)]
    (PTree. ptree tpmap)))

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
                        (ct/currentContext))]
    (make-ptree :csneps.core/Conjunction (@down-cableset a))))
  