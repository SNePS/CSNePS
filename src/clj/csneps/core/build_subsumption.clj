(in-ns 'csneps.core.build)

;;; There are many issues still to be worked out. 
;;; - Do we want to keep these separate?
;;; - What will be the role of unification?

(def subsumption-lattice(ref '[]))

(defrecord2 TreeNode 
  [data nil 
   children (ref '[])])

(defn restriction-subset?
  "Does term1 have a subset of the restrictins of term2?"
  [term1 term2]
  (let [rs1 @(:restriction-set term1)]
    (every? #(not (nil? %))
            (map #(ct/asserted? 
                    (apply-sub-to-term % {term1 term2}) 
                    (ct/currentContext)) rs1))))

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



