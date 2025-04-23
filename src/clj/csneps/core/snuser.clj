;(set! *warn-on-reflection* true)

(ns csneps.core.snuser
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.snip :as snip]
            [csneps.snip.inference-graph.concurrent :as igc]
            [csneps.gui :as gui]
            [csneps.utils.ontology :as onto-tools]
            [clojure.tools.cli :refer [parse-opts]]
            [reply.main])
  (:use clojure.stacktrace)
  (:refer-clojure :exclude [+ - * / < <= > >= == not= assert find load exit quit])
  (:use [clojure.pprint :only (cl-format)]
        [clojure.core.memoize :only (memo-clear!)]
        [clojure.walk]
        [csneps.core.caseframes :only (find-frame list-caseframes sameFrame description)]
        [csneps.demo :only (demo)]
        [clojure.set :only (union difference)]
        [csneps.core.relations :only (list-slots)]
        [csneps.core.contexts :only (currentContext defineContext listContexts setCurrentContext remove-from-context)]
        [csneps.core.build :only (*PRECISION* defrule unassert rewrite-propositional-expr)]
        [csneps.core.find :only (find)]
        [csneps.core :only (showTypes list-types)]
        [csneps.core.semantic-types :only [semantic-type-of]]
        [csneps.core.printer :only (writeKBToTextFile)]
        [csneps.snip :only (definePath pathsfrom cancel-infer-of cancel-infer-from cancel-focused-infer
                                       define-primaction attach-primaction perform ig-debug-all)]
        [csneps.core.arithmetic]
        [csneps.utils.coreutils :only (synvariable?)]
        [csneps.util]
        [csneps.utils.dotgraph :only (generate-dotfile)]
        [csneps.debug :only (debug set-debug-nodes set-debug-features)])
  (:import [csneps.util CountingLatch])
  (:gen-class))

(declare askif askifnot defineTerm find-rule find-term)

(defn adopt-rule
  "Adopts the rule where rule is either the symbolic name of the rule or the rule wft itself."
  [rule]
  (let [rule-node (find-rule rule)]
    (if rule-node
      (when-let [taskid (snip/adopt rule-node)]
        (.await ^CountingLatch (@igc/infer-status taskid)))
      (error "Rule " rule " does not exist."))))

(defn adopt-rules
  "Takes a list of symbolic rule names to be adopted in order, one after the other.
   Rows may take the form of a single rule name, or a vector of rule names. A vector
   of rule names will be adopted simultaneously. Optionally pauses before each rule."
  [rules & {:keys [pause] :or {pause false}}]
  (let [keep-pausing (ref pause)
        continue (ref true)]
    (doseq [row rules]
      (when (and @continue @keep-pausing)
        (loop []
          (println "\n--- pause ---\n")
          (println "Next rule: " row)
          (let [usrinput (read-line)]
            (case usrinput
              "" (noop)
              "c" (dosync (ref-set keep-pausing nil))
              "q" (dosync (ref-set continue nil))
              ("?" "h") (do (cl-format true
                                       "~%The following commands are available at pause points:~
                                        ~%  h,?            Print this help message~
                                        ~%  l,^            Enter Clojure read/eval/print loop~
                                        ~%  c              Continue without pausing~
                                        ~%  q              Quit the rule adoption~
                                        ~%  RETURN         Adopt next rule~
                                        ~%")
                            (recur))
              ("l" "^") (do
                          (println "Rule adoption interrupted. Type exit and press enter to continue.")
                          (clojure.main/repl :read (fn [request-prompt request-exit]
                                                     (let [form (clojure.main/repl-read request-prompt request-exit)]
                                                       (if (= 'exit form) request-exit form))))
                          (recur))
              (recur)))))
      (when @continue
        (if (vector? row)
          (let [tasks (doall (map #(future (adopt-rule %)) row))]
            (doall (map deref tasks))))
        (adopt-rule row)))))

(defn assert [expr & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (rewrite-propositional-expr expr)
    (build/assert expr (currentContext))))

;; Attempts to resolve each symbol in expr, and if it can resolve it, evals the symbol.
;; This is useful when you have a term stored in a variable, and then want to assert something about it
;; but don't want to use Clojure's nasty escape syntax.
;; e.g.
;; (def term1 (assert '(Isa x y)))
;; (assert* '(Isa term1 Proposition))
;; yields: (Isa (Isa x y) Proposition)
;; This is equivalent, in this case, to saying:
;; (assert `(~'Isa ~term1 ~'Proposition))
;; But note, this uses (resolve ...) to try to resolve the list elements. If one is a built-in function or variable
;; like: cat, then you should quote the input.
(defn assert* [expr & {:keys [precision]}]
  (binding [*PRECISION* (or precision *PRECISION*)]
    (rewrite-propositional-expr expr)
    (build/assert
      (vec (for [e expr]
             (if (and (symbol? e) (resolve e))
               (eval e)
                e))) (currentContext))))

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
  (if (some synvariable? (flatten exprpat))
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
  (snip/askif (build/variable-parse-and-build exprpat :Proposition #{})
              (currentContext)
              nil))

(defn askifnot
  "Returns a set of instances of the negation of the term pattern exprpat
         that are derivable in the current context;
         or the empty set if there are none."
  [exprpat]
  (rewrite-propositional-expr (list 'not exprpat))
  (snip/askif (build/variable-parse-and-build (list 'not exprpat) :Proposition #{})
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
      (and (seq? frame) (symbol? (first frame))) (seq frame)
      (and (seq? (first frame)) (= (ffirst frame) 'quote)) (seq (rest frame))
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
      (build/variable-parse-and-build term (or (first semtype) :Propositional) #{}))
    :else
    (build/build term (or (first semtype) :Entity) {} #{})))

(defmacro defineType
  "Semantic types are stored internally as keywords, so they are converted to keywords first if
   they are not already. Thisconversion maintains some backward compatibility with SNePS 3."
  [newtype supers & docstring]
  `(let [newtypekey# (keyword '~newtype)
         newsuperkeys# (map keyword '~supers)]
     (dosync (build/define-type newtypekey# newsuperkeys#))
     (println newtypekey# " defined as a subtype of " newsuperkeys#)))

(defmacro describe-terms
  "Prints a description of all the given terms."
  [& wftnames]
  `(doseq [tname# '~wftnames]
     (println (description (find-term tname#)))))

(defn find-rule
  "Finds the CARule associated with rule. If rule is already a CARule, just returns it. If rule is a wft name of a
   CARule it will find the rule and return it. If rule is the name of a rule, it will find the rule and return it.
   Otherwise nil."
  [rule]
  (if (isa? (csneps/syntactic-type-of rule) :csneps.core/CARule)
    rule
    (let [given-node (find-term rule)
          rule-node (if (and given-node (isa? (csneps/syntactic-type-of given-node) :csneps.core/CARule))
                    given-node
                    (let [rules (filter #(isa? (csneps/syntactic-type-of %) :csneps.core/CARule) (vals @csneps/TERMS))]
                      (first (filter #(= rule (:name (ffirst (@csneps/down-cableset %)))) rules))))]
      rule-node)))

(defn find-term
  [term]
  ;; Cast to a string before a symbol since numbers cannot be converted directly to symbols.
  (csneps/get-term (symbol (str term))))

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
  [& {:keys [asserted types originsets properties ontology]}]
  (let [terms (vals @csneps/TERMS)
        atoms (sort-by :name (filter #(= (:type %) :csneps.core/Atom) terms))
        arbs (sort-by :name (filter csneps/arbitraryTerm? terms))
        inds (sort-by :name (filter csneps/indefiniteTerm? terms))
        qvars (sort-by :name (filter csneps/queryTerm? terms))
        mols (sort-by :name (filter csneps/molecularTerm? terms))
        print-term (fn [x]
                     (when types (print (csneps/syntactic-type-of x) "-" (semantic-type-of x) " "))
                     (if (and properties (@csneps/property-map x)) (print (@csneps/property-map x) " ") #{})
                     (print x)
                     (when originsets (print " " (@csneps/support x)))
                     (println))]
    (doseq [x (concat atoms arbs inds qvars mols)]
      (let [asserted-in-ct (ct/asserted? x (ct/currentContext))
            ontological-term (ct/ontology-term? x)]
        (cond
          (and (not asserted) (not ontology) (not ontological-term)) (print-term x)
          (and asserted asserted-in-ct ontology) (print-term x)
          (and asserted asserted-in-ct (not ontology) (not ontological-term)) (print-term x)
          (and (not asserted) ontology) (print-term x))))))

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

(defn goaltrace
  []
  (reset! snip/goaltrace true))

(defn nogoaltrace
  []
  (reset! snip/goaltrace false))

(defn unadopt-rule
  [rule]
  (let [rule-node (find-rule rule)]
    (snip/unadopt rule-node)))

(defn verboserules
  "Enable verbose output for condition-action rules. Optionally takes a function of two arguments, rule name and rule
   bindings, which can be used to format output in custom ways."
  ([verbosefn]
   (reset! build/verbose-rules verbosefn))
  ([]
   (verboserules true)))

(defn noverboserules
  []
  (reset! build/verbose-rules false))

(defn startGUI
  ([] (gui/startGUI))
  ([termset] (gui/startGUI termset)))

(defn load
  [fname]
  (load-file fname))

(defn quit
  []
  (shutdown-agents)
  (igc/shutdownExecutor)
  (System/exit 0))

(defn exit [] (quit))

(clojure.core/load "/csneps/core/initialize")

(def cli-options
  ;; An option with a required argument
  [["-c" "--cli"]                                           ;; Use CLI (if nil, use GUI)
   ["-h" "--help"]])                                        ;; Help

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (when (:help options)
      (println summary)
      (System/exit 0))
    (igc/startExecutor)
    (clearkb true)
    (if (:cli options)
      (reply.main/launch {:custom-eval '(in-ns 'csneps.core.snuser)})
      (do
        (.start (Thread. gui/startGUI))
        (reply.main/launch {:custom-eval '(in-ns 'csneps.core.snuser)})))))

