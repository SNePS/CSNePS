(in-ns 'csneps.core.build)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Definition ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn- define-type*
  "Defines a new semantic type in the semantic type hierarchy. Only to be used during
   initialization when caseframes etc have not been initialized, meaning define-type 
   would fail." 
  [newtype supers]
  (dosync (alter semantic-type-hierarchy derive-list newtype supers)))

(defn assert-type-generic
  [newtype supers]
  (let [arb (build-variable (list 'every 'x (name newtype)))
        type-generic (assert (list 'Isa (:name arb) (set (map name supers))) 'OntologyCT)
        ;; This will have already been asserted but it's the easiest way to find it. 
        analytic-term (assert (list 'Isa (:name arb) (name newtype)) 'OntologyCT)]
    ;; Set up channel in semtype-in-channels.
    (let [sem-channel (build-channel nil analytic-term {} {})]
      (dosync
        (alter semtype-in-channels assoc analytic-term #{sem-channel})
        (alter semtype-to-channel-map assoc newtype sem-channel)
        (alter semtype-to-arb-map assoc newtype arb)))
    type-generic))

(defn define-type 
  "Defines a new semantic type. Takes as arguments a new type, and a list of supertypes."
  [newtype supers] 
  (define-type* newtype supers)
  (assert-type-generic newtype supers))

(defn initialize-default-hierarchy []
  (dosync
    (ref-set semtype-in-channels {})
    (ref-set semtype-to-channel-map {})
    (ref-set type-support {})
    (ref-set semantic-type-hierarchy (make-hierarchy))
	  (define-type* :Propositional '(:Entity))
	  (define-type* :WhQuestion '(:Propositional))
	  (define-type* :Proposition '(:Propositional))
	  (define-type* :Act '(:Entity))
	  (define-type* :Policy '(:Entity))
	  (define-type* :Thing '(:Entity))
	  (define-type* :Category '(:Thing))
	  (define-type* :Action '(:Thing))))

(defn initial-semtypes-to-obj-lang
  []
  (doseq [[c ps] (:parents @semantic-type-hierarchy)]
    (assert-type-generic c ps)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Adjustment ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn set-term-type
  [term newtype & {:keys [support] :or {support []}}]
  (dosync
    (alter type-map assoc (:name term) newtype)
    (when semtype-objectlang-experimental
      (st/add-type-support term newtype support))
    (when (and semtype-objectlang-experimental (@semtype-to-channel-map newtype))
      (submit-to-channel (@semtype-to-channel-map newtype)
                       (msg/new-message {:pos 1
                                     :type 'I-INFER
                                     :subst {(@semtype-to-arb-map newtype) term}})))))

(defn adjust-type
  "Adjusts the type of term, if possible,
       from its old semantic type, oldtype,
       to its new semantic type, newtype,
    and returns the term."
  [term oldtype newtype]
  ;(println "Adjusting type of: " (:name term) " from: " oldtype " -> " newtype)
  (cond
    ;; Types are already the same
    (= oldtype newtype) nil
    ;; Arbitrary terms can be adjusted down only while they're being built.
    ;; They can't ever be adjusted down below their highest-level restriction.
    (and (arbitraryTerm? term)
         @(:fully-built term)
         (not (subtypep oldtype newtype)))
    (error "Cannot adjust an arbitrary term " (:name term) " from type " oldtype " to more specific type " newtype ".")
    (and (queryTerm? term)
         @(:fully-built term)
         (not (subtypep oldtype newtype)))
    (error "Cannot adjust a query term " (:name term) " from type " oldtype " to more specific type " newtype ".")
    ;; Can't adjust WhQuestion to a subtype of Proposition.
    (and (whquestion-term? term)
         (subtypep newtype :Proposition))
    (error "Cannot adjust a WhQuestion to Proposition or lower.")

    ;; Newtype is a subtype of oldtype
    (subtypep newtype oldtype)
    (set-term-type term newtype)
    ;; If new type and oldtype have a common subtype,
    ;;    change the type to be the greatest common subtype
    ;; Could there be more than 1 greatest common subtype?
    :else
    (if-let [gcsub (gcsubtype newtype oldtype)]
      (if (> (count gcsub) 1)
        (let [gcsubchoice (menuChooseFromList (str "Choose a type for " term ".") gcsub)]
          (if gcsubchoice
            (set-term-type term gcsubchoice)
            (error "No semantic type for " term ".")))
        (set-term-type term (first gcsub)))
      (error (str "Type Error: Cannot adjust " (:name term) " from " oldtype " to " newtype "."))))
  ;; Propositions are true in contexts where they are hyps.
  (when (and (nil? (@support term))
             (or
               (subtypep newtype :Proposition)
               (subtypep newtype :Policy)))
    (dosync (alter support assoc term #{['hyp #{(:name term)}]})))
  term)