(ns csneps.utils.ontology
  (:require [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot]
            [clojure.set :as set]))

(defn- anonclass?
  [term]
  (or (= (@csneps.core/caseframe term) (cf/find-frame 'ObjectSomeValuesFrom))
      (= (@csneps.core/caseframe term) (cf/find-frame 'ObjectAllValuesFrom))
      (= (@csneps.core/caseframe term) (cf/find-frame 'ObjectHasValue))))

(defn- supertype-of
  [term]
  (let [ucs (@csneps.core/up-cablesetw term)
        wfts (when (get ucs (slot/find-slot 'subClassExpression))
               @(get ucs (slot/find-slot 'subClassExpression))) 
        supertypes (apply set/union (map #(second (@csneps.core/down-cableset %)) wfts))
        supertypes (remove anonclass? supertypes)
        supertype (first supertypes)];; Assume single inheritance
    supertype))

(defn- subtypes-of
  [term]
  (let [ucs (@csneps.core/up-cablesetw term)
        wfts (when (get ucs (slot/find-slot 'superClassExpression))
               @(get ucs (slot/find-slot 'superClassExpression))) 
        subtypes (apply set/union (map #(first (@csneps.core/down-cableset %)) wfts))
        subtypes (remove anonclass? subtypes)]
    subtypes))

(defn- dist-to-root
  [term]
    (loop [term term
           dist 0]
      (let [supertype (supertype-of term)]
        (if supertype
          (recur supertype (inc dist))
          dist))))

(defn- defined? 
  [term]
  nil
  )

(defn- siblings-of
  [term]
  (let [supertype (supertype-of term)]
    (remove #{term} (subtypes-of supertype))))

(defn- get-roots
  "A root is a concrete term which is a supertype, but not a subtype."
  [frame-terms]
  (let [subtypes (apply set/union (map #(first (@csneps.core/down-cableset %)) frame-terms))
        supertypes (apply set/union (map #(second (@csneps.core/down-cableset %)) frame-terms))
        roots (set/difference supertypes subtypes)]
    (remove anonclass? roots)))

(defn definition-order 
  "Returns a list of ontology terms without definitions, sorted 
   according to the algorithm in Schlegel & Elkin @ IWOOD 2015" 
  []
  (if-not (cf/find-frame 'SubClassOf)
    nil
    (let [subclassframe (cf/find-frame 'SubClassOf)
          frame-terms @(:terms subclassframe)
          roots (get-roots frame-terms)
          terms (apply set/union (map #(apply set/union (@csneps.core/down-cableset %)) frame-terms))
          terms (remove anonclass? terms)
          terms (remove defined? terms)
          dist-term-map (apply merge-with concat (map #(hash-map (dist-to-root %) [%]) terms))
          distances (sort (remove #{0} (keys dist-term-map)))]
      (apply concat
             (apply concat 
                    (for [i distances
                          :let [terms (get dist-term-map i)]]
                      (sort-by #(count (filter defined? %))
                               (loop [terms terms
                                      siblingsets []]
                                 (if (nil? (first terms))
                                   siblingsets
                                   (let [term (first terms)
                                         siblingset (conj (set (siblings-of term)) term)]
                                     (recur (remove siblingset (rest terms))
                                            (conj siblingsets siblingset))))))))))))

(defn define-next-term
  []
  (first (definition-order)))