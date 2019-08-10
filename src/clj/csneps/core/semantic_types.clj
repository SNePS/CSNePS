(ns csneps.core.semantic-types
  (:use [csneps.util]
        [csneps.configuration])
  (:require [csneps.core :as csneps]
            [csneps.core.contexts :as ct]))

(declare add-type-support)

;; This is old code which seems to only be used for variables. I'm not sure why we really need it to be separate
;; and I think we should integrate it somehow.

(defn instantiate-sem-type
  [term type]
  (let [termname (:name term)
        newtypekey (keyword type)]
    (dosync (alter csneps/type-map assoc termname newtypekey))
    (when semtype-objectlang-experimental
      (add-type-support term type []))
    ;;If the type is a descendent of Proposition it has a support set, hcontext set, and supported
    ;;nodes set.
    (when (csneps/subtypep newtypekey :Proposition)
      (dosync
        (alter csneps/support-set assoc termname (ref (hash-set)))
        (alter csneps/supported-nodes-set assoc termname (ref (hash-set)))))
    ;;If the type is an Act or Action, it has a nil primaction to start.
    (when (or (csneps/subtypep newtypekey :Act) (isa? @csneps/semantic-type-hierarchy newtypekey :Action))
      (dosync
        (alter csneps/primaction assoc termname nil)))
    newtypekey))

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
        type (cond
               (> (count types) 1) (first (reduce csneps/gcsubtype types))
               (= (count types) 1) (first types)
               :default (error (str "Term " (:name term) " has no type in context " (:name context))))]
    (if-not type
      (error (str "Term " (:name term) " has inconsistent type in context " (:name context)))
      type)))

(defn semantic-type-of
  [term]
  (if semtype-objectlang-experimental
    (semtype-in-context term (ct/currentContext))
    (if (isa? (csneps/type-of term) :csneps.core/Term)
      (get @csneps/type-map (:name term))
      (get @csneps/type-map term))))