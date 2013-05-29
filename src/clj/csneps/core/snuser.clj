;(set! *warn-on-reflection* true)

(ns csneps.core.snuser
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.snip :as snip]
            [csneps.gui :as gui])
  (:use clojure.stacktrace)
  (:refer-clojure :exclude [+ - * / < <= > >= == not= assert find load])
  (:use [clojure.pprint :only (cl-format)]
        [clojure.walk]
        [csneps.core.caseframes :only (list-caseframes sameFrame description)]
        [csneps.demo :only (demo)]
        [csneps.core.relations :only (list-slots)]
        [csneps.core.contexts :only (currentContext defineContext listContexts setCurrentContext remove-from-context)]
        [csneps.core.build :only (find unassert *PRECISION*)]
        [csneps.core :only (showTypes semantic-type-of)]
        [csneps.core.printer :only (writeKBToTextFile)]
        [csneps.snip :only (definePath pathsfrom)]
        [csneps.core.arithmetic]
        [csneps.util]))

(declare askif askifnot defineTerm find-term)

(defn assert [expr & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (let [assertion (build/assert expr (currentContext) :hyp)]
      (snip/submit-assertion-to-channels assertion)
      assertion)))

(defn assert! [expr & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (snip/forward-infer (build/build expr :Proposition {}))))

(defn assertAll [exprs & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (doseq [expr exprs] (assert expr))))

(defn ask
  "Returns a set of instances of the term pattern exprpat or its negation
        that are derivable in the current context;
        or the empty set if there are none."
  [exprpat]
  (setOr				; Until exprpat can be non-ground --- then set:union
    (askif exprpat)
    (askifnot exprpat)))

(defn askif
  "Returns a set of instances of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  [exprpat]
  (snip/askif (build/build exprpat :Proposition {}) (currentContext) nil))

(defn askifnot
  "Returns a set of instances of the negation of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  [exprpat]
  (snip/askif
    (build/build (list 'not exprpat) :Proposition {})
    (currentContext)
     nil))

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
    `(do 
       (slot/define-slot '~name ~@call)
       (if (get ~zm :path)
         (definePath '~name (get ~zm :path))))))

(defn defineCaseframe
  [type frame & {:keys [docstring fsymbols] :or {docstring ""}}]
    ;(println frame (first frame) (clojure.core/type (first frame)) (clojure.core/type (first (first frame))))
    (cf/define-caseframe
      type
      (cond
        (and (seq? frame) (symbol? (first frame)))   (seq frame)
        (and (seq? (first frame)) (= (first (first frame)) 'quote))  (seq (rest frame))
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
    (build/variable-parse-and-build term :Propositional)
    :else
    (build/build term (or semtype :Entity) {})))

(defmacro defineType
  [newtype parents & docstring]
  `(do
    (dosync (csneps/define-type '~newtype '(~@parents)))
    (str '~newtype " defined as a subtype of " '~@parents)))

(defmacro describe-terms
  "Prints a description of all the given terms."
  [& wftnames]
  (doseq [tname wftnames]
     (println (description (find-term tname)))))

(defn find-term
  [term]
  (csneps/get-term (symbol term)))

(defn list-variables
  "Prints the variable nodes. First arbitraries and then indefinites. If the 
   types keyword is not nil, then it prints the types of each term."
  [& {:keys [types]}]
  (doseq [arb @csneps/ARBITRARIES]
    (cl-format true "~:[~*~;<~S> ~]~S~%" 
               types (type arb) arb))
  (doseq [arb @csneps/INDEFINITES]
    (cl-format true "~:[~*~;<~S> ~]~S~%" 
               types (type arb) arb)))

(defn list-terms
  ""
  [& {:keys [asserted types]}]
  (doseq [x (vals @csneps/TERMS)]
    (when (or (not asserted)
              (and asserted
                   (ct/asserted? x (ct/currentContext))))
      (when types (print (csneps/syntactic-type-of x) "-" (csneps/semantic-type-of x) " "))
      (println x))))

(defn listkb
  "Prints the current context and all propositions asserted in it."
  []
  (println (:name (ct/currentContext)))
  (doseq [i (str (:name (ct/currentContext)))]
    (print "-"))
  (println)
  (list-terms :asserted true))

(defn startGUI
  ([] (gui/startGUI))
  ([termset] (gui/startGUI termset)))

(defn load
  [fname]
  (clojure.lang.Compiler/loadFile fname))

(clojure.core/load "/csneps/core/rules")
(clojure.core/load "/csneps/core/initialize")

(clearkb true)

(defn -main [& args]
  (set! *print-length* 40)
  (set! *print-level* 4)
  (gui/startGUI))

