(in-ns 'csneps.snip)

;;; CSNePS Acting System

(def primaction-fns (ref {}))

(defn adopt 
  "Adopt a policy."
  [policy]
  {:pre [(csneps/subtypep (csneps/semantic-type-of policy) :Policy)]}
  (let [ct (ct/currentContext)
        taskid (gensym "task")]
    (when-not (ct/asserted? policy ct)
      (dosync (alter (:hyps ct) conj policy))
      (when (isa? (csneps/syntactic-type-of policy) :csneps.core/CARule)
        (backward-infer policy taskid)
        taskid))))

(defn unadopt
  "Unadopt a policy."
  [policy]
  {:pre [(csneps/subtypep (csneps/semantic-type-of policy) :Policy)]}
  (let [ct (ct/currentContext)]
    (when (ct/asserted? policy ct)
      (dosync (alter (:hyps ct) disj policy))
      (when (isa? (csneps/syntactic-type-of policy) :csneps.core/CARule)
        (cancel-infer-of policy)))))

(defn attach-primaction [act fname]
  {:pre [(csneps/subtypep (csneps/semantic-type-of act) :Act)
         @(primaction-fns fname)]}
  (dosync (alter csneps/primaction assoc act @(primaction-fns fname))))