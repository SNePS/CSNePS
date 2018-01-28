(in-ns 'csneps.core.build)

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
    (dosync (alter semtype-in-channels assoc analytic-term #{(build-channel nil analytic-term {} {})}))
    type-generic))

(defn define-type 
  "Defines a new semantic type. Takes as arguments a new type, and a list of supertypes."
  [newtype supers] 
  (define-type* newtype supers)
  (assert-type-generic newtype supers))

(defn initialize-default-hierarchy []
  (dosync (ref-set semantic-type-hierarchy (make-hierarchy))
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