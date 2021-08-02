(ns csneps.core
  (:require [clojure.pprint]
            [clojure.set]
            [clojure.string :as st]
            [clojure.set :as set])
  (:use [csneps.util]))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSNePS Data Model ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Terms and their counts.
(def TERMS
  "A map from term names to the actual terms"
  (ref (hash-map)))

(def ARBITRARIES
  "The set of all arbitrary individual nodes."
  (ref #{}))

(def INDEFINITES
  "The set of all arbitrary individual nodes."
  (ref #{}))

(def QVARS
  "The set of all question-mark nodes."
  (ref #{}))

(def WFTCOUNT (ref 0))
(def ARBCOUNT (ref 0))
(def INDCOUNT (ref 0))
(def QVARCOUNT (ref 0))

;; Propositional Graph
(def up-cablesetw (ref {}))
(def down-cableset (ref {}))
(def restriction-set (ref {}))

;; Properties of terms in the graph
(def term-caseframe-map (ref {}))
(def support (ref {}))
(def dependencies (ref {}))

(def property-map
  "Maps the term to its set of properties."
  (ref (hash-map)))

(def support-set
  "Maps the term name to it's support set."
  (ref (hash-map)))

(def supported-nodes-set
  "Maps the term name to its supported node set."
  (ref (hash-map)))

(def primaction
  "Maps a term to it's primaction (Act/Actor only)."
  (ref (hash-map)))

;; Inference Graph extensions to the Propositional Graph
(def i-channels (ref {}))
(def u-channels (ref {}))
(def g-channels (ref {}))
(def ant-in-channels (ref {}))

;; Inference graph data
(def future-fw-infer (ref {}))
(def msgs (ref {}))
(def lattice-node (ref {}))
(def instances (ref {}))
(def expected-instances (ref {}))

;; Semantic types in the object language
(def semtype-in-channels (ref {})) ;; Like the other channel maps.
(def semtype-to-channel-map (ref {})) ;; Maps semtypes to the channel for that type.
(def semtype-to-arb-map (ref {})) ;; Maps semtypes to the arbitrary for that type.

(def type-support
  "Maps the term name to a map with a vector of usages for each semantic type."
  (ref (hash-map)))

;; Syntactic Types
(def TopSyntacticType
  "The root of the syntactic type hierarchy."
  ::Term)

;; Semantic Types
(def TOP-SEMANTIC-TYPE :Entity)

(def semantic-type-hierarchy (ref nil))

;; Goal is to eliminate this with putting the types in the object language.
(def type-map
  "Maps the term name to the semantic type."
  (ref (hash-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility Functions on Data Model ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn caseframe-for
  "Returns the caseframe for term."
  [term]
  (@term-caseframe-map term))

(defn part-of-terms
  "Returns the terms which a given term is part of."
  [term]
  (apply set/union  (map deref (vals (@up-cablesetw term)))))

;; Load the rest of the csneps.core namespace.
(load "core_syntactic_types")
(load "core_semantic_types")