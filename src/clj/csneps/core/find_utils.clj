(ns csneps.core.find-utils
  (:use [csneps.core]
        [csneps.util])
  (:require [csneps.core.relations :as slot]))

(defn findto
  "Returns the set of nodes to which a slot, r, goes from n, including
    possibly the empty set."
  [n r]
  (let [rel (if (symbol? r)
              (slot/find-slot r)
              r)]
    (when (isa? (type-of n) :csneps.core/Molecular)
      (let [pos (first (positions #{rel} (:slots (@caseframe n))))]
        (if pos
          (nth (seq (@down-cableset n)) pos)
          #{})))))

(defn findfrom
  "Returns the set of nodes
        from which a slot r, or a slot named r, goes to m."
  [m r]
  {:pre [(term? m)]}
  (let [res (get (@up-cablesetw m) (if (= (type r) csneps.core.relations.Slot) r (slot/find-slot r)))]
    (if res @res (hash-set))))