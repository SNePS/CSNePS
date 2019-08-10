(in-ns 'csneps.core)

;;; There are really (structurally) only 3 different types - Entity, Proposition, Act(ion).
;;; Proposition is an Entity with a support-set, and supported-nodes-set
;;; Act/Action is an Entity/Thing with a primaction

(declare type-of subtypep)

(defn genericTerm? 
  [o]
  (when-let [pset (@property-map o)]
    (pset :Generic)))

(defn genericAnalyticTerm? 
  [o]
  (when-let [pset (@property-map o)]
    (and (pset :Generic) (pset :Analytic))))

(defn analyticTerm? 
  [o]
  (when-let [pset (@property-map o)]
    (and (pset :Analytic))))

;;; Functions on Semantic Types
;;; ============================

(defn list-types
  []
  "Lists all the semantic types."
  (when-not (nil? @semantic-type-hierarchy) (println TOP-SEMANTIC-TYPE))
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

(defn subtypep
  "Checks if type1 is a descendent of type2"
  [type1 type2]
  (isa? @semantic-type-hierarchy type1 type2))

(defn proper-subtypep
  "Checks if type1 is a proper subtype of type2"
  [type1 type2]
  (and (not= type1 type2) (subtypep type1 type2)))

(defn semtype?
  "Checks if a keyword is a semtype"
  [kw]
  ((conj (descendants @semantic-type-hierarchy :Entity) :Entity) kw))

(defn gcsubtype
  "Returns a set of the greatest common subtypes of type1 and type2"
  [type1 type2]
    ;(println "gcsubtype")
    (if (= type1 type2) (list type1)
      (let [common (clojure.set/intersection (set (conj (descendants @semantic-type-hierarchy type1) type1))
                                             (set (conj (descendants @semantic-type-hierarchy type2) type2)))
            result (ref #{})]
        (doseq [x (seq common)]
          (if (empty? (clojure.set/intersection (ancestors @semantic-type-hierarchy x) common))
            (dosync (ref-set result (conj @result x)))))
        (seq @result))))