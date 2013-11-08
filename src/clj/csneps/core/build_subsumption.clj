(in-ns 'csneps.core.build)

;;; There are many issues still to be worked out. 

;;; In my formulatuon of the lattice, there may be multiple roots.
;;; I don't believe there can be cycles, but I'm not sure. 
;;; Some TBox implementations in DL do allow for cycles - I'm not sure we want/need that.
;;; There is no "most general term" or "most specific term" shared by all terms.
(def subsumption-lattice(ref '[]))

(defrecord2 TreeNode 
  [data nil 
   parents (ref '[])
   children (ref '[])])

(defn restriction-subset?
  "Does term1 have a subset of the restrictins of term2?"
  [term1 term2]
  (let [rs1 @(:restriction-set term1)]
    (every? #(not (nil? %))
            (map #(ct/asserted? 
                    (apply-sub-to-term % {term1 term2}) 
                    (ct/currentContext)) rs1))))

(defn restriction-subset-list
  [terms term2]
  (for [term1 terms
        :when (restriction-subset? term1 term2)]
    term1))

(defn- find-lattice-parents
  [arb]
  ;; Explore the children of each item in nodes recursively.
  ;; If an item in nodes does not have any children that 
  ;; subsume arb, then the item is a parent of arb. If the
  ;; item in nodes does have children that subsume arb, those
  ;; are added to the nodes list. Since
  ;; arb can have multiple parents, we must continue until
  ;; nodes is empty. 
  (loop [nodes (restriction-subset-list @subsumption-lattice arb)
         parents nil] 
    (if (empty? nodes)
      parents
      (let [node (first nodes)
            satchildren (if (:children node)
                          (restriction-subset-list (:children node))
                          '())]
        (if (empty? satchildren)
          (recur
            (rest nodes)
            (conj parents node))
          (recur
            (concat (rest nodes) satchildren)
            parents))))))

(defn- adjust-lattice-children-helper
  [node parent]
  (let [pchildren @(:children parent)]
    (doseq [pchild pchildren]
      (when (restriction-subset? (:data node) (:data pchild))
        (dosync 
          (ref-set (:parents pchild)
                   (-> 
                     (->> @(:parents pchild) (remove #{parent}))
                     (conj node)))
          (alter (:children node) conj pchild))))))

(defn adjust-lattice-children
  [node parents] 
  ;; Move the children of each item in parents
  ;; which is now the child of node, by determining
  ;; if (:data node) subsumes each child of the parent.
  (map #(adjust-lattice-children-helper node %) parents))

(defn lattice-insert 
  [arb] 
  (let [parents (find-lattice-parents arb)
        node (new-tree-node {:data arb :parents parents})]
    (adjust-lattice-children node parents)))

(defn insert
  [arb]
  (loop [parent nil
         nodes @subsumption-lattice]
    (cond
      (empty? nodes)
      (dosync (alter (or (:children parent)
                         subsumption-lattice)
                     conj (new-tree-node {:data arb})))
      (restriction-subset? (:data (first nodes)) arb)
      (recur (first nodes)
             @(:children (first nodes)))
      :else
      (recur parent
             (rest nodes)))))
  

(defn structurally-subsumes-vars
  "var1 structurally subsumes var2 if var1 has a subset of
   var2's restrictions."
  [var1 var2]
  (restriction-subset? var1 var2))



