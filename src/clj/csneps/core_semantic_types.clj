(in-ns 'csneps.core)

(def TOP-SEMANTIC-TYPE :Entity)

(defvar semantic-type-hierarchy (ref nil))

;;Maps the term name to the semantic type
(defvar type-map (ref (hash-map)))

;;Maps the term name to it's support set
(defvar support-set (ref (hash-map)))

;;Maps the term name to its supported node set.
(defvar supported-nodes-set (ref (hash-map)))

;;Maps a term to it's primaction (Act/Actor only)
(defvar primaction (ref (hash-map)))

;;; There are really (structurally) only 3 different types - Entity, Proposition, Act(ion).
;;; Proposition is an Entity with a support-set, and supported-nodes-set
;;; Act/Action is an Entity/Thing with a primaction

(declare type-of subtypep)

;;; Functions on Semantic Types
;;; ============================

(defn list-types
  []
  "Lists all the semantic types."
  (when (not (nil? @semantic-type-hierarchy)) (println TOP-SEMANTIC-TYPE))
  (doseq [x (vec (descendants @semantic-type-hierarchy TOP-SEMANTIC-TYPE))]
    (println x))
  nil)

(defn showTypes
  "Lists all the semantic types."
  []
  (list-types))

(defn semantic-type-p
  "Returns t if name is the name of a SNePS semantic type;
     else returns nil."
  [name]
  (isa? @semantic-type-hierarchy name TOP-SEMANTIC-TYPE))

(defn define-type
  ""
  [newtype supers]
  (let [newtypekey (if (keyword? newtype) newtype (keyword newtype))]
    (dosync (alter semantic-type-hierarchy derive-list newtypekey supers))))

(defn instantiate-sem-type
  [termname type]
  (let [newtypekey (if (keyword? type) type (keyword type))]
    (dosync (alter type-map assoc termname newtypekey))
    ;;If the type is a descendent of Proposition it has a support set, hcontext set, and supported
      ;;nodes set.
      (when (subtypep newtypekey :Proposition)
          (dosync
            (alter support-set assoc termname (ref (hash-set)))
            (alter supported-nodes-set assoc termname (ref (hash-set)))))
      ;;If the type is an Act or Action, it has a nil primaction to start.
      (when (or (subtypep newtypekey :Act) (isa? @semantic-type-hierarchy newtypekey :Action))
        (dosync
          (alter primaction assoc termname nil)))
      newtypekey))

(defn semantic-type-of
  [term]
  (if (isa? (type-of term) ::Term)
    (get @type-map (:name term))
    (get @type-map term)))

(defn initialize-default-hierarchy []
  (dosync (ref-set semantic-type-hierarchy (make-hierarchy))
	  (define-type :Propositional '(:Entity))
	  (define-type :WhQuestion '(:Propositional))
	  (define-type :Proposition '(:Propositional))
	  (define-type :Act '(:Entity))
	  (define-type :Policy '(:Entity))
	  (define-type :Thing '(:Entity))
	  (define-type :Category '(:Thing))
	  (define-type :Action '(:Thing))))

(defn subtypep
  "Checks if type1 is a descendent of type2"
  [type1 type2]
  (isa? @semantic-type-hierarchy type1 type2))

(defn proper-subtypep
  "Checks if type1 is a proper subtype of type2"
  [type1 type2]
  (and (not (= type1 type2)) (subtypep type1 type2)))

(defn gcsubtype
  [type1 type2]
  "Returns a set of the greatest common subtypes of type1 and type2"
    ;(println "gcsubtype")
    (if (= type1 type2) (list type1)
      (let [common (clojure.set/intersection (set (conj (descendants @semantic-type-hierarchy type1) type1))
                                             (set (conj (descendants @semantic-type-hierarchy type2) type2)))
            result (ref #{})]
        (doseq [x (seq common)]
          (if (empty? (clojure.set/intersection (ancestors @semantic-type-hierarchy x) common))
            (dosync (ref-set result (conj @result x)))))
        (seq @result))))