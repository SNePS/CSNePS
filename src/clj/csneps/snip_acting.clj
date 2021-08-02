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
    vars should be a (possibly empty) vector of slot names
      that get bound to the appropriate nodes.
    However, if any var is enclosed in parentheses,
       it gets bound to some randomly selected member of the appropriate node set.
    forms syntax is just as it is for standard Clojure function definition."
  [name vars & forms]
  (let [stripped-vars (mapv #(if (seq? %) (first %) %) vars)
        act-node-var 'act-node-var
        primaction-fn `(fn [~act-node-var]
                        ~(if (empty? stripped-vars)
                          `((fn [] ~@forms))
                          `((fn ~stripped-vars ~@forms)
                            ~(mapv (fn [rel]
                                    (if (seq? rel)
                                      `(first (pb-findtos ~act-node-var (first '~rel)))
                                      `(pb-findtos ~act-node-var '~rel)))
                                  vars))
                          ))]
    (dosync (alter primaction-fns assoc name primaction-fn))
    primaction-fn))

(defn perform
  "actform will be defined as an Act term (which might already exist).
      If there is a primitive action function for that act, it will be applied to the act;
      Else if the act has an actions slot
          and there is a primitive action function for that action, it will be applied to the act;
      Else an error will be raised."
  [actform]
  (let [act (build/build actform :Act {} #{})
        todo (pb-findtos act 'action)
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

(defn attach-primaction [act fn-or-fname]
  {:pre [(or (subtypep (st/semantic-type-of act) :Act)
             (subtypep (st/semantic-type-of act) :Action))]}
  (let [prim-fn (cond
                  (fn? fn-or-fname) fn-or-fname
                  (@primaction-fns fn-or-fname) (eval (@primaction-fns fn-or-fname))
                  :default (error "I can't find the primitive action function " fn-or-fname))]
    (dosync (alter primaction assoc act prim-fn)))
  nil)



