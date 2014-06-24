;;; sneps3_printer.clj
;;; A printer for SNePS 3 terms.
;;; Author: Daniel R. Schlegel
;;; Modified: 6/10/2014

(ns csneps.core.printer
  (:require [csneps.core.contexts :as ct]
            [csneps.core :as csneps]
            [csneps.core.caseframes :as cf])
  (:use [csneps.core :only (type-of)]
        [clojure.pprint :only (cl-format pprint)]
        [csneps.util]
        [clojure.java.io]))

(def ^{:dynamic true} PRINTED-VARIABLES (hash-set))

(declare print-term print-set)

(defn print-atom
  [term]
  (:name term))

(defn wft-string
  [term]
  (str (:name term)
      (if (csneps/subtypep (csneps/semantic-type-of term) :Proposition)
        (if (ct/asserted? term (ct/currentContext)) "!" "?"))":"))

(defn args-str
  [args]
  (map #(print-term %) args))

(defn print-negation
  [args]
  (str (cons (if (= (count args) 1) 'not 'nor) (args-str args))))

(defn print-negationbyfailure
  [args]
  (str (cons (if (= (count args) 1) 'thnot 'thnor) (args-str args))))

(defn print-param2op
  [fcn min max args]
  (str (list fcn (list min max) (args-str args))))

(defn print-nary
  [fcn args]
  (str (cons fcn (args-str args))))

(defn print-molecular
  [cf cs]
  (str "(" 
         (apply str 
                (interpose " " 
                           (if (cf/hasOneArgumentSlot cf)
                             (let [fsym (first (:print-pattern cf))]
                               (if (and (seq? fsym)
                                        (= 'quote (first fsym)))
                                 (cons (second fsym) (if (set? (first cs)) 
                                                       (map #(print-term %) (first cs)) 
                                                       (list (print-term (first cs)))))
                                 (cons (print-term (first cs)) (if (set? (second cs)) 
                                                                 (map #(print-term %) (second cs)) 
                                                                 (list (print-term (second cs)))))))
                             (for [p (:print-pattern cf)]
                               (cond
                                 (and (seq? p) (= (first p) 'quote))
                                 (second p)
                                 (symbol? p)
                                 (print-term (nth cs (first (positions #(= % p) (map #(:name %) (:slots cf))))))
                                 :else 
                                 (error "Bad pattern part "p" in the pattern "(:print-pattern cf)"."))))))
         ")"))


(defn print-unnamed-variable-term
  [term]
  (str
    (cond
      (PRINTED-VARIABLES term) (:var-label term)
      :true
      (do 
        (set! PRINTED-VARIABLES (conj PRINTED-VARIABLES term))
        (print-str 
          (condp = (type-of term)
            :csneps.core/Arbitrary (str "(every " (:var-label term) " ")
            :csneps.core/Indefinite (str "(some " (:var-label term) " (" (print-set (@csneps/dependencies term) false) ") ")
            :csneps.core/QueryVariable (str "(" (:var-label term) " "))
          (print-set (@csneps/restriction-set term) false) ")")))))

(defn print-closure
  [term]
  (print-str (list 'close (map #(:var-label %) (:closed-vars term)) (print-term (first (@csneps/down-cableset term))))))

(defn print-unnamed-molecular-term
  [term]
  ;(println "Printing term: " term)
  (condp = (type-of term)
    :csneps.core/Closure
      (print-closure term)
    :csneps.core/Negation
      (print-negation (first (@csneps/down-cableset term)))
    :csneps.core/Negationbyfailure
      (print-negationbyfailure (first (@csneps/down-cableset term)))
    :csneps.core/Conjunction
      (print-nary 'and (first (@csneps/down-cableset term)))
    :csneps.core/Disjunction
      (print-nary 'or (first (@csneps/down-cableset term)))
    :csneps.core/Equivalence
      (print-nary 'iff (first (@csneps/down-cableset term)))
    :csneps.core/Xor
      (print-nary 'xor (first (@csneps/down-cableset term)))
    :csneps.core/Nand
      (print-nary 'nand (first (@csneps/down-cableset term)))
    :csneps.core/Andor
      (print-param2op 'andor (:min term) (:max term) (first (@csneps/down-cableset term)))
    :csneps.core/Thresh
      (print-param2op 'thresh (:min term) (:max term) (first (@csneps/down-cableset term)))
    :csneps.core/Implication
      (print-str (list 'if (print-term (first (@csneps/down-cableset term))) (print-term (second (@csneps/down-cableset term)))))
    :csneps.core/Numericalentailment
      (print-str (list (symbol (str "=" (if (= (:min term) 1) :v (:min term)) ">"))
                       (first (@csneps/down-cableset term)) (second (@csneps/down-cableset term))))
    :csneps.core/Arbitrary
      (print-unnamed-variable-term term)
    :csneps.core/Indefinite
      (print-unnamed-variable-term term)
    :csneps.core/QueryVariable
      (print-unnamed-variable-term term)
    (print-molecular (@csneps/caseframe term) (@csneps/down-cableset term))
    ))

(defn sneps-printer
  [x]
  (condp = (type x)
    csneps.core.Atom (print-atom x *out*)
    csneps.core.Categorization (print-molecular x *out*)
    (pprint x *out*)))

;(defn print-named-molecular-term
;  "Prints to the stream
;        the molecular term preceded by its wft name
;        and an indication of its assert status."
;  [term stream]
;  (cl-format stream "~@<~W~:[~*~;~:[?~;!~]~]: ~W~:>"
;	  (:name term)
;	  (= (csneps/semantic-type-of term) :Proposition)
;	  (ct/asserted? term (ct/currentContext)) term))

(defn print-named-variable-term
  [term]
  (str (:name term) ": " (print-unnamed-variable-term term)))


;(defmethod print-method csneps.core.Categorization [o w]
;  (print-named-molecular-term o w))

(defn print-set
  [slotset show-setof?]
  (if (= (count slotset) 1)
    (print-term (first slotset))
    (if show-setof?
      (str "#{" (apply str (interpose " " (for [n slotset] (print-term n)))) "}")
      (str (apply str (interpose " " (for [n slotset] (print-term n))))))))

(defn print-term
  [term]
  ;(println "Term: " term)
  (condp = (type-of term)
    clojure.lang.PersistentHashSet (print-set term true)
    :csneps.core/Atom (print-atom term)
    (print-unnamed-molecular-term term)))

(defn term-printer
  [term]
  (binding [PRINTED-VARIABLES (hash-set)]
  (cond
    ;; Variable
    (isa? (csneps.core/syntactic-type-of term) :csneps.core/Variable)
    (print-str (print-named-variable-term term))
    ;; Molecular
    (isa? (csneps.core/syntactic-type-of term) :csneps.core/Molecular)
    (print-str (wft-string term) (print-term term))
    ;; Term Set
    (set? term)
    (print-set term true)
    :else
    (str (print-atom term)
      (if
        (and (= (csneps.core/semantic-type-of term) :Proposition)
	     (ct/asserted? term (ct/currentContext)))
        "!" "")))))

(defn sneps-printer
  [object]
  (if (isa? (csneps.core/syntactic-type-of object) :csneps.core/Term)
    (pprint (term-printer object))
    (pprint object)))

(defn sneps-printer-str
  [object]
  (if (isa? (csneps.core/syntactic-type-of object) :csneps.core/Term)
    (with-out-str (pprint (term-printer object)))
    (with-out-str (pprint object))))

(defn sexpr-printer
  [object]
  (cond
    (seq? object)
    (for [o object]
      (if (:name o)
        (:name o)
        (sexpr-printer o)))
    (set? object)
    (set (for [o object]
      (if (:name o)
        (:name o)
        (sexpr-printer o))))
    :else object))

(defn unif-printer
  [ulist]
  ;(println ulist)
  (for [m ulist]
    (into {} (map
      (fn [[k v]]
        [
         (if (isa? (csneps.core/syntactic-type-of k) :csneps.core/Variable)
           (print-atom k)
           (sexpr-printer k))
         (if (isa? (csneps.core/syntactic-type-of v) :csneps.core/Variable)
           (print-atom v)
           (sexpr-printer v))
         ]) m))))

;;; Print-method functions
(defmethod print-method csneps.core.Term [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Atom [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Base [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Variable [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Arbitrary [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Indefinite [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.QueryVariable [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Molecular [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Closure [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Carule [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Param2op [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Andor [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Disjunction [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Xor [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Nand [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Thresh [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Equivalence [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Conjunction [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Negation [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Negationbyfailure [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Numericalentailment [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Orentailment [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Implication [o w]
  (.write ^java.io.Writer w (str (term-printer o))))

(defmethod print-method csneps.core.Categorization [o w]
  (.write ^java.io.Writer w (str (term-printer o))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Writing CSNePS KBs to Files ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn writeKBToTextFile 
  "Writes the KB to the given text file,
   so that when that file is loaded,
   all the propositions asserted in the current KB
   will be asserted in the new KB.
   If the headerfile is included,
      a load of that file will be written before any of the asserts."
  [file & headerfile]
  (with-open [w ^java.io.Writer (writer file)]
    (.write w ";;; CSNePS KB\n")
    (.write w ";;; =========\n")
    (.write w (str ";;; " (.toString (new java.util.Date)) "\n"))
    (when headerfile 
      (.write ^java.io.Writer w "(clojure.lang.Compiler/loadFile " (first headerfile) ")\n"))
    (.write w ";;; Assumes that all required Contexts, Types, Slots, and Caseframes have now been loaded.\n(in-ns 'csneps.core.snuser)\n")
    (doseq [term (vals @csneps/TERMS)]
      (when (ct/asserted? term (ct/currentContext))
        (.write w  "(csneps.core.build/assert '")
        (if (= (:type term) :csneps.core/Atom)
          (.write w (str (print-atom term)))
          (binding [PRINTED-VARIABLES (hash-set)]
            (.write w (str (print-unnamed-molecular-term term)))))
        (.write w (str " 'DefaultCT)\n"))))))