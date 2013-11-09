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
    (let [rs1 @(:restriction-set term1)]
      (every? #(not (nil? %))
              (map #(ct/asserted? 
                      (apply-sub-to-term % {term1 term2}) 
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
            satparents (if (not (empty @(:parents node)))
                         (restriction-subset-nodelist arb @(:parents node))
                         '())]
        (if (empty? satparents)
          (recur
            (rest nodes)
            (conj children node))
          (recur
            (concat (rest nodes) satparents)
            children))))))

(defn- adjust-lattice-parents-helper
  "Given a to-be-child of node, determine which of its
   parents subsume node, and update the relationships between
   those parents and node."
  [node child]
  (let [cparents @(:parents child)]
    ;; If cparents are empty, then node is the new parent. 
    (if (empty? cparents)
      (dosync 
        (ref-set subsumption-lattice
                 (-> 
                   (->> @subsumption-lattice (remove #{child}))
                   (conj node)))
        (alter (:parents child) conj node))
      (doseq [cparent cparents]
        ;; When this matches, cparent is now a parent of node.
        ;; So, cparent needs to be removed as a parent of child,
        ;; and added as a parent of node.
        (println (:data cparent) ":" (:data node))
        (when (restriction-subset? (:data cparent) (:data node))
          (dosync 
            (ref-set (:parents child)
                     (->
                       (->> @(:parents child) (remove #{cparent}))
                       (conj node)))
            (ref-set (:children cparent)
                     (->
                       (->> @(:children cparent) (remove #{child}))
                       (conj node)))
            (alter (:parents node) conj cparent)))))))
  
(defn adjust-lattice-parents
  [node children] 
  ;; Move the parents of each item in children
  ;; which is now the parent of node
  (doseq [c children]
    (adjust-lattice-parents-helper node c)))

(defn lattice-insert 
  [arb] 
  (let [children (find-lattice-children arb)
        node (new-lattice-node {:data arb :children (ref children)})]
    (doseq [c children] (println "Child:" (:data c)))
    (dosync (ref-set (:lattice-node arb) node))
    (if (empty? children)
      ;; From the parents of omega, determine if 
      ;; any of them subsume this node.
      (let [parents (restriction-nodelist-subset @(:parents omega) arb)]
        (dosync 
          (ref-set (:children node) #{omega})
          (alter (:parents omega) conj node))
        ;(println parents)
        (if (empty? parents)
          (dosync (alter subsumption-lattice conj node))
          (doseq [p parents]
            (dosync 
              (ref-set (:children p) #{node})
              (alter (:parents node) conj p)
              (ref-set (:parents omega) (remove #{p} @(:parents omega)))))))
      ;; Children need updating.
      (do 
        ;; 
        (adjust-lattice-parents node children)
        ;; Node is now the parent of each child.
        (dosync
          (doseq [c children]
            (alter (:children node) conj c)))))))

(defn structurally-subsumes-vars
  "var1 structurally subsumes var2 if var1 has a subset of
   var2's restrictions."
  [var1 var2]
  (restriction-subset? var1 var2))

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

