;(set! *warn-on-reflection* true)

(ns csneps.core.snuser
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.snip :as snip]
            [csneps.gui :as gui]
            [csneps.utils.ontology :as onto-tools])
  (:use clojure.stacktrace)
  (:refer-clojure :exclude [+ - * / < <= > >= == not= assert find load])
  (:use [clojure.pprint :only (cl-format)]
        [clojure.core.memoize :only (memo-clear!)]
        [clojure.walk]
        [csneps.core.caseframes :only (list-caseframes sameFrame description)]
        [csneps.demo :only (demo)]
        [clojure.set :only (union)]
        [csneps.core.relations :only (list-slots)]
        [csneps.core.contexts :only (currentContext defineContext listContexts setCurrentContext remove-from-context)]
        [csneps.core.build :only (find *PRECISION* defrule unassert rewrite-propositional-expr)]
        [csneps.core :only (showTypes semantic-type-of)]
        [csneps.core.printer :only (writeKBToTextFile)]
        [csneps.snip :only (definePath pathsfrom cancel-infer-of cancel-infer-from cancel-focused-infer adopt unadopt attach-primaction)]
        [csneps.core.arithmetic]
        [csneps.util])
  (:import [edu.buffalo.csneps.util CountingLatch]))

(declare askif askifnot defineTerm find-term)

(defn adopt-rule
  "Adopts the rule with the symbol rule-name as its name."
  [rule-name]
  (let [rules (filter #(isa? (csneps/syntactic-type-of %) :csneps.core/CARule) (vals @csneps/TERMS))
        rule (filter #(= rule-name (:name (ffirst (@csneps/down-cableset %)))) rules)]
    (if (first rule)
      (when-let [taskid (adopt (first rule))]
        (.await ^CountingLatch (@snip/infer-status taskid)))
      (error "Rule " rule-name " does not exist."))))

(defn adopt-rules 
  "Takes a list of symbolic rule names to be adopted in order, one after the other. 
   Rows may take the form of a single rule name, or a vector of rule names. A vector
   of rule names will be adopted simultaneously."
  [order]
  (doseq [row order]
    (if (vector? row)
      (let [tasks (doall (map #(future (adopt-rule %)) row))]
        (doall (map deref tasks)))
      (adopt-rule row))))

(defn assert [expr & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (rewrite-propositional-expr expr)
    (build/assert expr (currentContext))))

(defn assert! [expr & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (rewrite-propositional-expr expr)
    (let [term (build/assert expr (currentContext))]
      (snip/forward-infer term)
      term)))

(defn assertAll [exprs & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (doseq [expr exprs] (assert expr))))

(defn ask
  "Returns a set of instances of the term pattern exprpat or its negation
        that are derivable in the current context;
        or the empty set if there are none."
  [exprpat]
  (if (some build/synvariable? (flatten exprpat))
    (snip/askwh-instances exprpat (ct/currentContext))
    (setOr				
      (askif exprpat)
      (askifnot exprpat))))

(defn askif
  "Returns a set of instances of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  [exprpat]
  (rewrite-propositional-expr exprpat)
  (snip/askif (build/variable-parse-and-build exprpat :Proposition)  
              (currentContext) 
              nil))

(defn askifnot
  "Returns a set of instances of the negation of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  [exprpat]
  (rewrite-propositional-expr (list 'not exprpat))
  (snip/askif (build/variable-parse-and-build (list 'not exprpat) :Proposition)
              (currentContext)
              nil))

(defn askwh
  ""
  [exprpat]
  (snip/askwh exprpat (currentContext)))

(defn allTerms [& {:keys [test] :or {test identity}}]
  "Returns a set of all the terms in the knowledge base."
  (set (filter test (vals @csneps/TERMS))))

(defmacro defineSlot
  [name & args]
  (let [kws (take-nth 2 args)
        vals (take-nth 2 (rest args))
        qtvals (map (fn [v] `'~v) vals)
        call (interleave kws qtvals)
        zm (zipmap kws qtvals)]
    `(let [slot# (slot/define-slot '~name ~@call)]
       (if (get ~zm :path)
         (definePath '~name (get ~zm :path)))
       slot#)))

(defn defineCaseframe
  [type frame & {:keys [docstring fsymbols] :or {docstring ""}}]
    ;(println frame (first frame) (clojure.core/type (first frame)) (clojure.core/type (first (first frame))))
    (cf/define-caseframe
      type
      (cond
        (and (seq? frame) (symbol? (first frame)))   (seq frame)
        (and (seq? (first frame)) (= (ffirst frame) 'quote))  (seq (rest frame))
        :else (error "The frame, ~S, must be a list of slot names or ~
                                    a list containing a quoted atomic constant followed by slot names."))
      :docstring docstring
      :print-pattern frame
      :fsymbols fsymbols))

(defn defineTerm
  "Finds or builds the given term,
    assures that it is over the given semantic type,
    and returns the term."
  [term & semtype]
  (cond
    (and (seq? term) (or (= (first term) 'some)
                         (= (first term) 'every)))
    (build/build-variable term)
    (seq? term)
    (do 
      (rewrite-propositional-expr term)
      (build/variable-parse-and-build term :Propositional))
    :else
    (build/build term (or (first semtype) :Entity) {})))

(defmacro defineType
  [newtype parents & docstring]
  `(do
    (dosync (csneps/define-type '~newtype '(~@parents)))
    (str '~newtype " defined as a subtype of " '~@parents)))

(defmacro describe-terms
  "Prints a description of all the given terms."
  [& wftnames]
  `(doseq [tname# '~wftnames]
     (println (description (find-term tname#)))))

(defn find-term
  [term]
  (csneps/get-term (symbol term)))

(defn list-focused-inference-tasks
  []
  (let [fw-tasks (apply union (vals @csneps/future-fw-infer))
        bw-tasks (apply union (map #(deref (:future-bw-infer %)) (vals @csneps/TERMS)))]
    (when (seq bw-tasks) (println "Attempting to derive:"))
    (doseq [t bw-tasks]
      (println t))
    (when (seq fw-tasks) (println "Deriving from:"))
    (doseq [t fw-tasks]
      (println t))))

(defn list-variables
  "Prints the variable nodes. First arbitraries and then indefinites. If the 
   types keyword is not nil, then it prints the types of each term."
  [& {:keys [types]}]
  (doseq [arb @csneps/ARBITRARIES]
    (cl-format true "~:[~*~;<~S> ~]~S~%" 
               types (type arb) arb))
  (doseq [ind @csneps/INDEFINITES]
    (cl-format true "~:[~*~;<~S> ~]~S~%" 
               types (type ind) ind))
  (doseq [qvar @csneps/QVARS]
    (cl-format true "~:[~*~;<~S> ~]~S~%" 
               types (type qvar) qvar)))

(defn list-terms
  ""
  ;; First print atomic terms;
  ;; Then arbitrary nodes
  ;; Then indefinite nodes
  ;; Then qvar nodes
  ;; Then print molecular terms;
  [& {:keys [asserted types originsets properties]}]
  (let [terms (vals @csneps/TERMS)
        atoms (sort-by :name (filter #(= (:type %) :csneps.core/Atom) terms))
        arbs (sort-by :name (filter csneps/arbitraryTerm? terms))
        inds (sort-by :name (filter csneps/indefiniteTerm? terms))
        qvars (sort-by :name (filter csneps/queryTerm? terms))
        mols (sort-by :name (filter csneps/molecularTerm? terms))]
    (doseq [x (concat atoms arbs inds qvars mols)]
      (when (or (not asserted)
                (and asserted
                     (ct/asserted? x (ct/currentContext))))
        (when types (print (csneps/syntactic-type-of x) "-" (csneps/semantic-type-of x) " "))
        (if (and properties (@csneps/property-map x)) (print (@csneps/property-map x) " ") #{})
        (print x)
        (when originsets (print " " (@csneps/support x)))
        (println)))))

(defn listkb
  "Prints the current context and all propositions asserted in it."
  []
  (println (:name (ct/currentContext)))
  (doseq [i (str (:name (ct/currentContext)))]
    (print "-"))
  (println)
  (list-terms :asserted true))

(defn krnovice
  [b]
  (dosync (ref-set build/KRNovice b)))

(defn startGUI
  ([] (gui/startGUI))
  ([termset] (gui/startGUI termset)))

(defn load
  [fname]
  (load-file fname))

(clojure.core/load "/csneps/core/initialize")
(clojure.core/load "/csneps/test/benchmark")
(clojure.core/load "/csneps/test/mapper_benchmark")

(clearkb true)

(defn -main [& args]
  (set! *print-length* 40)
  (set! *print-level* 4)
  (gui/startGUI))

