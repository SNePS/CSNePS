(in-ns 'csneps.snip)

(declare adopt-subrules unadopt-subrules add-valve-selector remove-valve-selector)

;;; CSNePS Acting System

(def primaction-fns (ref {}))

(defn adopt 
  "Adopt a policy."
  [policy]
  {:pre [(subtypep (semantic-type-of policy) :Policy)]}
  (let [ct (ct/currentContext)
        taskid (gensym "task")]
    (when-not (ct/asserted? policy ct)
      (dosync (alter (:hyps ct) conj policy))
      (when (isa? (syntactic-type-of policy) :csneps.core/CARule)
        (backward-infer policy taskid)
        (doseq [ich (@i-channels policy)]
          (add-valve-selector ich {} (ct/currentContext) taskid))
        (adopt-subrules policy)
        taskid))))

(defn adopt-subrules
  [policy]
  (doseq [subrule (get (cf/dcsRelationTermsetMap policy) (slot/find-slot 'subrule))]
    (adopt subrule)))

(defn unadopt
  "Unadopt a policy."
  [policy]
  {:pre [(subtypep (semantic-type-of policy) :Policy)]}
  (let [ct (ct/currentContext)]
    (when (ct/asserted? policy ct)
      (dosync (alter (:hyps ct) disj policy))
      (when (isa? (syntactic-type-of policy) :csneps.core/CARule)
        (cancel-infer-of policy)
        (doseq [ich (@i-channels policy)]
          (remove-valve-selector ich {} (ct/currentContext) taskid))
        (unadopt-subrules policy)))))

(defn unadopt-subrules 
  [policy]
  (doseq [subrule (get (cf/dcsRelationTermsetMap policy) (slot/find-slot 'subrule))]
    (unadopt subrule)))

(defn attach-primaction [act fname]
  {:pre [(subtypep (semantic-type-of act) :Act)
         @(primaction-fns fname)]}
  (dosync (alter primaction assoc act @(primaction-fns fname))))