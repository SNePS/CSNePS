(in-ns 'csneps.core.build)

(declare lattice-insert submit-assertion-to-channels build-quantterm-channels adjustType)

(defn variable-parse-and-build
  "Given a top-level build expression, checks that expression for
   variable terms syntax (e.g., every, some). These terms are built and
   a new build expression is created with just the variable-labels
   (e.g., x in (every x ...)). This and a substitution between
   variable-labels and the AI and IO is provided to build. Also asserts into
   the Base KB the restriction sets. Returns the built expression."
  [expr type]
  (let [[new-expr vars substitution] (check-and-build-variables expr)]
    (doseq [v (seq vars)]
      (doseq [rst (seq @(:restriction-set v))]
        (when-not (isa? (semantic-type-of rst) :WhQuestion) ;; It doesn't make sense to assert a WhQuestion.
          (assert rst (ct/find-context 'BaseCT))))
      (build-quantterm-channels v)
      (when (= (syntactic-type-of v) :csneps.core/Arbitrary) (lattice-insert v)))
    (build new-expr type substitution)))

(defn build-variable 
  "This function should only be called when a single variable needs to be built
   indepndent of an assert. It is in assert because variable nodes need to assert 
   their restriction sets. Returns the variable node built."
  [var-expr]
  (let [[new-expr vars substitution] (check-and-build-variables var-expr)]
    (doseq [rst (seq @(:restriction-set (first vars)))]
      (assert rst (ct/find-context 'BaseCT)))
    (build-quantterm-channels (first vars))
    (first vars)))

(defn check-contradiction
  "Raise a warning if the newly asserted proposition, p
   constitutes a contradiction in the given context."
  [p context]
  (let [negs (findfrom p (slot/find-slot 'nor))]
    (doseq [n negs]
      (when (ct/asserted? n context)
        (println "Warning:" p "and" n "contradict!"))))
  (let [negs (findto p (slot/find-slot 'nor))]
    (doseq [n negs]
      (when (ct/asserted? n context)
        (println "Warning:" p "and" n "contradict!")))))

(defmulti assert
  (fn [expr context] [(type-of expr)]))

(defmethod assert
  [clojure.lang.Symbol] [expr context]
  (assert (build expr :Proposition {}) context))

(defmethod assert
  [java.lang.Integer] [expr context]
  (assert (build expr :Proposition {}) context))

(defmethod assert
  [java.lang.Double] [expr context]
  (assert (build expr :Proposition {}) context))

(defmethod assert
  [java.lang.String] [expr context]
  (assert (build expr :Proposition {}) context))

(defmethod assert
  [clojure.lang.PersistentList] [expr context]
  (assert (variable-parse-and-build expr :Proposition) context))

(defmethod assert
  [clojure.lang.Cons] [expr context]
  (assert (variable-parse-and-build (seq expr) :Proposition) context))

(defmethod assert
  [clojure.lang.PersistentVector] [expr context]
  (assert (variable-parse-and-build (seq expr) :Proposition) context))

(defn assert-term
  [expr context]
  (let [ct (csneps.core.contexts/find-context context)]
    (when-not (ct/asserted? expr ct)
      (dosync (alter (:hyps ct) conj expr))
      (adjustType expr (semantic-type-of expr) :Proposition)
      (submit-assertion-to-channels expr))
    (check-contradiction expr ct))
  expr)

(defmethod assert
  ;[:Proposition] [expr context origintag]
  [:csneps.core/Term] [expr context]
  (let [ct (csneps.core.contexts/find-context context)]
    (when-not (ct/asserted? expr ct)
      (dosync (alter (:hyps ct) conj expr))
      (adjustType expr (semantic-type-of expr) :Proposition)
      (submit-assertion-to-channels expr))
    (check-contradiction expr ct))
  expr)

(defn unassert
  "Unasserts the proposition prop in the given context and all ancestor contexts."
  [prop & [ctx]]
  ;; Currently there is no belief revision,
  ;;    so propositions derived using prop might still be asserted,
  ;;    and prop, itself, might be rederivable.
  (let [cntx (or ctx (ct/currentContext))
        p (build prop :Proposition {})]
    (loop [context (ct/asserted? p cntx)]
      (when context
        (ct/remove-from-context p context)
        (recur (ct/asserted? p cntx))))))

(defn add-to-context
  "Adds the term to the context's hyps."
  [term ctx]
  (dosync
    (alter (:hyps ctx) conj (build term :Proposition {}))))