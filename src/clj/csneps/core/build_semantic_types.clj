(in-ns 'csneps.core.build)

(defn define-type
  "Defines a new semantic type. Takes as arguments a new type, and a list of supertypes."
  [newtype supers]
  (dosync (alter semantic-type-hierarchy derive-list newtype supers)))

(defn initialize-default-hierarchy []
  (dosync (ref-set semantic-type-hierarchy (make-hierarchy))
	  (define-type :Propositional '(:Entity))
	  ;(define-type :WhQuestion '(:Propositional))
	  (define-type :Proposition '(:Propositional))
	  (define-type :Act '(:Entity))
	  (define-type :Policy '(:Entity))
	  (define-type :Thing '(:Entity))
	  (define-type :Category '(:Thing))
	  (define-type :Action '(:Thing))))