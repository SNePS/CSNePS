(ns csneps.core.semantic-types
  (:use [csneps.util])
  (:require [csneps.core :as csneps]
            [csneps.core.contexts :as ct]))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Term type support ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-type-support
  "Adds a vector of terms supporting a semantic type of a term to the type-support map."
  [term sem-type supporting-terms]
  (let [types (@csneps/type-support (:name term))
        new-supports (set (conj (sem-type types) supporting-terms))]
    (dosync (alter csneps/type-support assoc (:name term) (assoc types sem-type new-supports)))))

(defn supported-type?
  "Returns true if the type is directly supported in the provided context. No inference is
   used to get less specific type support."
  [term sem-type context]
  (let [supports (sem-type (@csneps/type-support (:name term)))]
    (some #(every? (fn [t] (ct/asserted? t context)) %) supports)))

(defn supported-types
  "Returns the list of supported semantic types of a term in the provided context."
  [term context]
  (remove nil?
          (map #(when (supported-type? term % context) %) (keys (@csneps/type-support (:name term))))))

(defn semtype-in-context
  "Returns the current type of a term in the given context."
  [term context]
  (let [types (supported-types term context)
        type (if (> (count types) 0)
               (reduce csneps/gcsubtype types)
               (error (str "Term" term "has no type in context" (:name context))))]
    (when-not type
      (error (str "Term" term "has inconsistent type in context" (:name context))))))

(defn semantic-type-of
  [term]
  (if (isa? (csneps/type-of term) :csneps.core/Term)
    (get @csneps/type-map (:name term))
    (get @csneps/type-map term)))