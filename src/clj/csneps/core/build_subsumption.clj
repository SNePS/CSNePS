(in-ns 'csneps.core.build)

(declare omega)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lattice Structure ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I don't believe there can be cycles, but I'm not sure. 
;;; Some TBox implementations in DL do allow for cycles -
;;; I'm not sure we want/need that.
(defrecord2 LatticeNode 
  [data nil 
   parents (ref #{})
   children (ref #{})])

;;; omega is subsumed by every term - it is more specific than
;;; every thing else, by definition.
(def omega (new-lattice-node {:data 'OMEGA}))

;;; In my formulatuon of the lattice, there may be multiple roots, 
;;; there is no 'most general term'.
(def subsumption-lattice (ref [omega]))

(defn reset-lattice
  []
  (dosync 
    (ref-set (:parents omega) #{})
    (ref-set (:children omega) #{})
    (ref-set subsumption-lattice [omega])))

;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn restriction-subset?
  "Does term1 have a subset of the restrictins of term2?"
  [term1 term2]
  (cond 
    (= term2 'OMEGA)
    true
    (= term1 'OMEGA)
    false
    :else
    (let [rs1 (@restriction-set term1)]
      (every? #(not (nil? %))
              (map #(ct/asserted? 
                      (apply-sub-to-term % {term1 term2} true) 
                      (ct/currentContext)) rs1)))))

(defn restriction-nodelist-subset
  [nodes term2]
  (for [node nodes
        :when (restriction-subset? (:data node) term2)]
    node))

(defn restriction-subset-nodelist
  [term1 nodes]
  (for [node nodes
        :when (restriction-subset? term1 (:data node))]
    node))

(defn- find-lattice-children
  [arb]
  ;; Explore the parents of omega recursively.
  ;; If an item in nodes does not have any parents that 
  ;; subsume arb, then the item is a child of arb. If the
  ;; item in nodes does have parents that subsume arb, those
  ;; are added to the nodes list. Since
  ;; arb can have multiple children, we must continue until
  ;; nodes is empty. 
  (loop [nodes (restriction-subset-nodelist arb @(:parents omega))
         children '#{}]
    (if (empty? nodes) 
      children
      (let [node (first nodes)
            satparents (if (seq @(:parents node))
                         (restriction-subset-nodelist arb @(:parents node))
                         '())]
        (if (empty? satparents)
          (recur
            (rest nodes)
            (conj children node))
          (recur
            (concat (rest nodes) satparents)
            children))))))


(defn- find-lattice-parents
  [arb]
  ;; Explore the children of each item in nodes recursively.
  ;; If an item in nodes does not have any children that 
  ;; subsume arb, then the item is a parent of arb. If the
  ;; item in nodes does have children that subsume arb, those
  ;; are added to the nodes list. Since
  ;; arb can have multiple parents, we must continue until
  ;; nodes is empty. 
  (loop [nodes (restriction-nodelist-subset @subsumption-lattice arb)
         parents '#{}] 
    (if (empty? nodes)
      parents
      (let [node (first nodes)
            satchildren (if (seq @(:children node))
                          (restriction-nodelist-subset @(:children node) arb)
                          '())]
        (if (empty? satchildren)
          (recur
            (rest nodes)
            (conj parents node))
          (recur
            (concat (rest nodes) satchildren)
            parents))))))

(defn lattice-insert 
  [arb] 
  (if (nil? @(:lattice-node arb))
    (let [parents (find-lattice-parents arb)
          children (find-lattice-children arb)
          node (new-lattice-node {:data arb 
                                  :parents (ref parents)
                                  :children (ref (if (empty? children)
                                                   #{omega}
                                                   children))})]
      (dosync (ref-set (:lattice-node arb) node))
      (if (empty? parents)
        (dosync (alter subsumption-lattice conj node))
        (dosync
          (doseq [p parents]
            (ref-set (:children p) 
                     (conj (remove (conj children omega) @(:children p)) node)))))
      (if (empty? children)
        (dosync (alter (:parents omega) conj node))
        (dosync
          (doseq [c children]
            (ref-set (:parents c)
                     (conj (remove parents @(:parents c)) node))))))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subsumption Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns true if term1 subsumes term2.
(defmulti subsumes?
  (fn [term1 term2] [(syntactic-type-of term1) (syntactic-type-of term2)]))

(defmethod subsumes?
  [:csneps.core/Arbitrary :csneps.core/Arbitrary] [term1 term2]
  (restriction-subset? term1 term2))

(defmethod subsumes?
  [:csneps.core/QueryVariable :csneps.core/Arbitrary] [term1 term2]
  (restriction-subset? term1 term2)) ;; ??? I'm not sure.

(defmethod subsumes?
  [:csneps.core/QueryVariable :csneps.core/QueryVariable] [term1 term2]
  nil) ;; ??? I'm not sure.

(defmethod subsumes?
  [:csneps.core/Arbitrary :csneps.core/QueryVariable] [term1 term2]
  (restriction-subset? term1 term2)) ;; ??? I'm not sure.

(defmethod subsumes?
  [:csneps.core/Indefinite :csneps.core/Indefinite] [term1 term2]
  (restriction-subset? term1 term2))

(defmethod subsumes?
  [:csneps.core/Indefinite :csneps.core/Arbitrary] [term1 term2]
  nil)

(defmethod subsumes? 
  [:csneps.core/Arbitrary :csneps.core/Indefinite] [term1 term2]
  ;; Any elephant subsumes some albino elephant.
  (or 
    ;; Any elephant subsumes some albino elephant.
    (restriction-subset? term1 term2)
    ;; Any albino elephant subsumes some elephant.
    (restriction-subset? term2 term1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test Utility Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-lattice-contents
  []
  (doseq [arb @ARBITRARIES]
    (let [node @(:lattice-node arb)
          print-parents (for [p @(:parents node)] (:data p))
          print-children (for [c @(:children node)] (:data c))]
      (println (:data node) "- Parents:" print-parents "- Children:" print-children))))

