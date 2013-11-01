(in-ns 'csneps.core.build)

;;; There are many issues still to be worked out. 
;;; - Do we want to keep these separate?
;;; - What will be the role of unification?

(def subsumption-hierarchy (ref '[]))

(defrecord2 TreeNode 
  [data nil 
   children (ref '[])])

(defn restriction-subset?
  [term1 term2]
  (set/subset? @(:restriction-set term1) @(:restriction-set term2)))

(defn insert
  [arb]
  (loop [parent nil
         nodes @subsumption-hierarchy]
    (cond
      (empty? nodes)
      (dosync (alter (or (:children parent)
                         subsumption-hierarchy)
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
  (set/subset? (:restriction-set var1) (:restriction-set var2)))

(defn structurally-subsumes-terms
  "term1 structurally subsumes term2 if term1 is in a subset
   of the relations term2 is."
  [term1 term2]
  (let [t1rs @(:up-cablesetw term1)
        t2rs @(:up-cablesetw term2)]
    (every? 
      #(and (t2rs %)
            (set/subset? (t1rs %) (t2rs %)))
      (keys t1rs))))

(defn structurally-subsumes-varterm
  "var structurally subsumes term if term is in all of the
   relations that are in var's restriction set."
  [var term]
  (let [rs @(:restriction-set var)
        ucs (apply concat (map deref (vals @(:up-cablesetw term))))]
    (every? (fn [r] 
              (println r ucs)
              (cond
                ;; Equivalence is a good enough condition to accept on.
                (some #(= r %) ucs) true 
                ;; Recursive case - which is the matching term? This is naive.
                (arbitraryTerm? r) (some #(structurally-subsumes-varterm r %) ucs)
                ;; The term r contains a variable at some lower level. We should
                ;; try to unify r with each item in ucs. (Not yet implemented)
                (generic-term? r) false)) rs)))



