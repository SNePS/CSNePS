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

;; Moved from buildutils:
(defn ignore-variable? [sym] (= '_ sym))

(defn varinlist? [list] #(some #{%} list))

(def variable?
  (fn [term]
    (csneps/variableTerm? term)))

;(def variable? #(or (= (type-of %) :csneps.core/Arbitrary) (= (type-of %) :csneps.core/QueryVariable)))

(def synvariable? #(or (ignore-variable? %)
                       (and (symbol? %) (re-matches #"^\?.*" (name %)))))

(def syntype-fsym-map {:csneps.core/Negation 'not,
                       :csneps.core/Negationbyfailure 'thnot,
                       :csneps.core/Conjunction 'and,
                       :csneps.core/Disjunction 'or,
                       :csneps.core/Equivalence 'iff,
                       :csneps.core/Xor 'xor,
                       :csneps.core/Nand 'nand,
                       :csneps.core/Andor 'andor,
                       :csneps.core/Thresh 'thresh,
                       :csneps.core/Implication 'if})

(defn term-predicate
  [term]
  (or
    ((csneps/type-of term) syntype-fsym-map)
    (let [p (:print-pattern (@csneps/term-caseframe-map term))]
      (if (and (seq? (first p)) (= (first (first p)) 'quote))
        (second (first p))
        (:name (first (first (@csneps/down-cableset term))))))))