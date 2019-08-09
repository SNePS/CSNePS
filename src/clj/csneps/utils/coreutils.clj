;; Utils depending only on the CSNePS core.

(ns csneps.utils.coreutils
  (:require [csneps.core :as csneps]))

(defn- flatten-term-helper
  "Takes a term, and recursively explores its down-cablesets and optionally
   its restrictions to build a complete set of subterms."
  ([term seen vars?]
   (cond
     (seen term) '()
     (csneps/molecularTerm? term) (flatten (conj (map #(flatten-term-helper % (conj seen term) vars?) (@csneps/down-cableset term)) term))
     (and vars? (csneps/variableTerm? term)) (flatten (conj (map #(flatten-term-helper % (conj seen term) vars?) (@csneps/restriction-set term)) term))
     (csneps/atomicTerm? term) (list term)
     (set? term) (flatten (map #(flatten-term-helper % seen vars?) term)))))

;; vars? = true is obviously a little slower, so only do it when needed.
(defn flatten-term
  "Takes a term, and recursively explores its down-cablesets (and optionally
   restrictions) to build a complete set of subterms."
  [term & {:keys [vars?] :or {vars? false}}]
  (disj (set (flatten-term-helper term #{} vars?)) term))