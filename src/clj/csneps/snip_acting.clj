(in-ns 'csneps.snip)

(declare adopt-subrules unadopt-subrules add-valve-selector remove-valve-selector)

;;; CSNePS Acting System

(def primaction-fns (ref {}))

(defn adopt 
  "Adopt a policy."
  [policy]
  {:pre [(subtypep (st/semantic-type-of policy) :Policy)]}
  (let [ct (ct/currentContext)
        taskid (gensym "task")]
    (when-not (ct/asserted? policy ct)
      (ct/hypothesize policy ct)
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
  {:pre [(subtypep (st/semantic-type-of policy) :Policy)]}
  (let [ct (ct/currentContext)]
    (when (ct/asserted? policy ct)
      (ct/remove-from-context policy ct)
      (when (isa? (syntactic-type-of policy) :csneps.core/CARule)
        (cancel-infer-of policy)
        (doseq [ich (@i-channels policy)]
          (remove-valve-selector ich {} (ct/currentContext) taskid))
        (unadopt-subrules policy)))))

(defn unadopt-subrules 
  [policy]
  (doseq [subrule (get (cf/dcsRelationTermsetMap policy) (slot/find-slot 'subrule))]
    (unadopt subrule)))

;; This is a fairly direct port of the Sneps3 version, but I'm not sure it does everything we'd like it to.
;; Further analysis of the SNePS 2 SNeRE and other such systems should be performed before this is actively used.
(defmacro define-primaction
  "Creates the function definition of the primitive action named name.
    vars should be a (possibly empty) list of slot names
      that get bound to the appropriate nodes.
    However, if any var is enclosed in parentheses,
       it gets bound to a member of the appropriate node set.
    forms syntax is just as it is for standard Clojure function definition."
  [name vars & forms]

  )

(defn perform
  "actform will be defined as an Act term (which might already exist).
      If there is a primitive action function for that act, it will be applied to the act;
      Else if the act has an actions slot
          and there is a primitive action function for that action, it will be applied to the act;
      Else an error will be raised."
  [actform]
  (let [act (build/build actform :Act)
        todo (pb-findtos act 'actions)
        action (first todo)]
    (cond
      ;; This act has a primitive action associated with it.
      (fn? (@primaction act))
      ((@primaction act) act)
      ;; There is an action slot, and a function for that action.
      (and action
           (fn? (@primaction action)))
      ((@primaction action) act)
      ;; Error.
      :default
      (error "I don't know how to perform " act "."))))

(defn attach-primaction [act fname]
  {:pre [(subtypep (st/semantic-type-of act) :Act)
         @(primaction-fns fname)]}
  (dosync (alter primaction assoc act @(primaction-fns fname))))

