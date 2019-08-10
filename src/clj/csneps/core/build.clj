(ns csneps.core.build
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.printer :as print]
            [csneps.core.relations :as slot]
            [csneps.core.semantic-types :as st]
            [csneps.core.find :as find]
            [csneps.snip.messagestructure :as msgstruct]
            [csneps.snip.message :as msg]
            [csneps.debug :as debug]
            [clojure.core.match :as match]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as cb]
            [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.string]
            [csneps.core :as csneps])
  (:refer-clojure :exclude [assert]) ;;Possible bug: This breaks loading clojure.math.combinatorics for no reason?
  (:use [csneps.core]
        [csneps.configuration]
        [csneps.util]
        [csneps.utils.coreutils]
        [csneps.core.unify.treenode]
        [clojure.walk :as walk :only [prewalk prewalk-replace postwalk-replace]]
        [csneps.core.find-utils]))

;(refer-clojure :exclude '[assert])

(declare assert build check-and-build-variables build-channels build-unifier-channels build-quantterm-channels create-message-structure generic-term? check-and-build-variables)

(load "build_assert")
(load "build_utils")
(load "build_substitution")
(load "build_subsumption")
(load "build_unification")
(load "build_channel")
(load "build_rules")
(load "build_rewrite")
(load "build_semantic_types")

(def KRNovice
  "If the value is non-null,
      caseframes will be created automatically
        whenever the user uses a function symbol
            that doesn't already have a caseframe."
  (ref nil))

(def ^:dynamic *PRECISION* 5)

;; I'd rather this be in snuser or snip, but it's forced to be here for now.
(def verbose-rules
  "If non-nil, rules will output their bindings when fired."
  (atom nil))

(defn wftname?
  "Returns True if the input name looks like a wftname;
     else returns False."
  [name]
  (let [str (str name)]
    (and (> (.length str) 3)
         (= (.substring str 0 3) "wft")
         (every? digit-char-p (seq (subs str 3))))))

(defn quantterm?
  "Returns True if the input name looks like arbi or indi;
     else returns False."
  [name]
  (let [str (str name)]
    (and (> (.length str) 3)
         (or (= (.substring str 0 3) "arb") (= (.substring str 0 3) "ind"))
         (every? digit-char-p (seq (subs str 3))))))

(defn ientailsymb?
  "Returns t if the symbol name is v=> or i=>, for some positive integer i;
    nil otherwise."
  [symbol]
  (when (symbol? symbol)
    (let [fname (str symbol)
          len (.length fname)]
      (and (> len 2)
           (= (first fname) \=)
           (= (last fname) \>)
           (let [i (subs fname 1 (dec len))]
             (or (= i "v")
                 (every? digit-char-p i)))))))

(defn generic-term?
  "A term is a generic term if it has the property of being :Generic."
  [term]
  (when-let [pmap (@property-map term)]
    (pmap :Generic)))

(defn generic-fillers
  "Returns the generic fillers in term-or-termset. 
   The filler of a slot is generic if:
     1) It is a term with property :Generic,
     2) It is a term with syntactic type Arbitrary, or
     3) It is a set of terms in which any one term satisfies 1 or 2."
  [term-or-termset]
  (let [gt (fn [term] (when (or (generic-term? term) (isa? (type-of term) :csneps.core/Arbitrary))
                         term))]
    (if (set? term-or-termset)
      (remove nil? (doall (map gt term-or-termset)))
      (remove nil? (list (gt term-or-termset))))))

(defn whquestion-term?
  "A term is a whquestion if it has the property of being :WhQuestion."
  [term]
  (when-let [pmap (@property-map term)]
    (pmap :WhQuestion)))

(defn whquestion-fillers
  "Returns the whquestion fillers in term-or-termset. 
   The filler of a slot is a whquestion if:
     1) It is a term with property :WhQuestion,
     2) It is a term with syntactic type QueryVariable, or
     3) It is a set of terms in which any one term satisfies 1 or 2."
  [term-or-termset]
  (let [gt (fn [term] (when (or (whquestion-term? term) (isa? (type-of term) :csneps.core/QueryVariable))
                         term))]
    (if (set? term-or-termset)
      (remove nil? (doall (map gt term-or-termset)))
      (remove nil? (list (gt term-or-termset))))))

(defn ientaili
  "Assuming that i=> satisfies #'ientailsymb?,
       returns the number represented by i."
  [=i>]
  (let* [fname (str =i>)
         i (.substring fname 1 (dec (.length fname)))]
    (if (= i "v") 1 (Integer/parseInt i))))

(defn roundf
  "Returns a rounding of x according to *PRECISION*
       for use as a term name."
  [x]
  (float (/ (math/round (* x (math/expt 10 *PRECISION*)))
            (math/expt 10 *PRECISION*))))

(defn checkArity
  "Raises an error
       if the use of the function fn in the expression, expr,
       has the wrong arity according to fn's caseframe, cf.
    Otherwise, just returns nil."
  [fn expr cf]
  (let [correctlength (+ (count (:slots cf))
	                        (if (cf/quotedpp? cf) 1 0))]
    (when-not (= (count expr) correctlength)
      (error fn " is used with incorrect arity in " expr ".\n
                         It should be given " (dec correctlength) " arguments instead of " (dec (count expr)) "."))))

(defn check-min-max
  "Raises an error if fillers is a set
        that doesn't satisfy the min and max restrictions of slot."
  [fillers slot]
  (let [numfillers (cond
                     (nil? fillers) 0
                     (atom? fillers) 1
                     (set? fillers) (count fillers))
        min (:min slot)
        max (:max slot)]
    (when (and (number? min) (< numfillers min))
      (error
       "The set of fillers, " fillers ", is too few for the minimum restriction (" (:min slot) ") on the slot " (:name slot) "."))
    (when (and (number? max) (> numfillers max))
      (error
       "The set of fillers, " fillers ", is too many for the maximum restriction (" (:max slot) ") on the slot " (:name slot) "."))))

(defn build-molecular-node
  "Builds a molecular node with:
       the caseframe cf;
       the down-cableset dcs represented as a list;
       the syntactic type syntype;
       the contextually determined semantic type semtype;
       the semantic type, fsemtype,
           determined by the fillers of its slots
           (only used by rule nodes,
            the semantic types of whose arguments
            are to be used instead of the type proposition;
       the min parameter, given for andor and thresh;
       the max parameter, given for andor and thresh."
  [cf dcs syntype semtype & {:keys [fsemtype min max closed-vars properties]}]
  ;(println "building molecular..." dcs (doall (map make-set-if-not-set dcs)))
  (let [dcs-sets (map make-set-if-not-set dcs)
        tests (doall (map check-min-max dcs-sets (:slots cf)))
        existing-term (cond 
                        max (find/find-exact syntype cf dcs-sets :min min :max max)
                        min (find/find-exact syntype cf dcs-sets :min min)
                        :else (find/find-exact syntype cf dcs-sets))
        term (or
               existing-term
               (let [wft ((get-new-syntype-fn syntype) {:name (symbol (str "wft" (inc-wft-counter)))
                                                        :min (if (nil? min) 0 min)
                                                        :max (if (nil? max) 0 max)
                                                        :closed-vars closed-vars})]
                 (dosync 
                   (alter down-cableset assoc wft dcs-sets)
                   (alter term-caseframe-map assoc wft cf)
                   (alter msgs assoc wft (create-message-structure syntype dcs-sets :n min))
                   (alter TERMS assoc (:name wft) wft)
                   (set-term-type wft (:type cf)))
                 
                 (initialize-syntype wft)
                 
                 (cf/add-caseframe-term wft :cf cf)
                 wft))]

    (adjust-type term (or (@type-map (:name term)) (:type cf)) (if fsemtype fsemtype semtype))
    
    (when properties 
      (dosync (alter property-map assoc term (set/union (@property-map term) properties))))
    
    ;;Now that we made it, add it to the unif tree, unify it, and build appropriate channels.
    (when (and (not existing-term)
               (#{:csneps.core/Molecular :csneps.core/Categorization
                  :csneps.core/Closure :csneps.core/Negation} (:type term)))
      (doseq [unif (match term)]
        (build-unifier-channels unif))
      (addTermToUnificationTree term))
    
    term))
  

(defn build-andor
  "Build a term for andor
       args is the original expression after 'andor'."
  [args semtype subst]
  (let [cf (cf/find-frame 'andor)]
    (when-not cf (error "There is no frame associated with andor."))
    (when-not (cl-seqable? (first args))
      (error "andor must be followed by a list, (i j) in " (list* 'andor args) "."))
    (let [min (ffirst args)
          max (second (first args))
          tot (count (rest args))]
      (when-not (and (integer? min) (<= 0 min tot))
	(error
	 "The min parameter of andor must be an integer ~
              between 0 and the number of arguments, " tot ", in " (list* 'andor args) "."))
      (when-not (and (number? max) (<= min max tot))
	(error
	 "The max parameter of andor must be an integer ~
              between " min " and " tot ", in " (list* 'andor args) "."))
      (cond
       ;; Canonical transformations
        (and (zero? min) (= max tot))
          (build 'True semtype {} #{})
        (= tot min max)
          (build (list* 'and (rest args)) semtype subst #{})
        (= 0 min max)
          (build (list* 'nor (rest args)) semtype subst #{})
        (and (zero? min) (= max (dec tot)))
          (build (list* 'nand (rest args)) semtype subst #{})
        :else
          (let [fillers (build (list* 'setof (rest args)) semtype subst #{})
                term (build-molecular-node
                       cf (list fillers) :csneps.core/Andor semtype
                       :fsemtype (st/semantic-type-of fillers)
                       :min min :max max)]
            (build-channels term)
            term)))))

(defn build-thresh
  "Build a term for thresh.
       args is the original expression after 'thresh'."
  [args semtype subst]
  (let [cf (cf/find-frame 'thresh)]
    (when-not cf (error "There is no frame associated with thresh."))
    (when-not (cl-seqable? (first args))
      (error
       "thresh must be followed by a list, (i) or (i j), in "
         (list* 'thresh args) "."))
    (let [min (ffirst args)
          tot (count (rest args))
          tot-1 (dec tot)
          max (or (second (first args)) tot-1)]
      (when-not (and (integer? min) (<= 0 min tot))
        (error
          "The min parameter of thresh must be an integer ~
              between 0 and " tot-1 ", in ~S " (list* 'thresh args) "."))
      (when-not (and (number? max) (<= min max tot))
        (error
          "The max parameter of thresh must be an integer ~
              between " min " and " tot ", in " (list* 'thresh args) "."))
      (cond
       ;; Canonical transformations
        (and (zero? min) (= max tot))
          (build 'False semtype {} #{})
        (and (zero? min) (= max (dec tot)))
          (build (list* 'and (rest args)) semtype subst #{}) 
        (= min max 0)
          (build (list* 'or (rest args)) semtype subst #{})
        (and (= min 1) (= max tot))
          (build (list* 'nor (rest args)) semtype subst #{})
        (= min tot)
          (build (list* 'nand (rest args)) semtype subst #{})
        :else 
          (let [fillers (build (list* 'setof (rest args)) semtype subst #{})
                term (build-channels 
                       (build-molecular-node
                         cf (list fillers) :csneps.core/Thresh semtype
                         :fsemtype (st/semantic-type-of fillers)
                         :min min :max max))]
            term)))))

(defn build-numerical-entailmant
  "Builds the term for `(i=> ant cq)."
  [i ant cq semtype substitution]
  (let [ant (if (= (type ant) clojure.lang.PersistentHashSet)
               ant
               (hash-set ant))
        cq (if (= (type cq) clojure.lang.PersistentHashSet)
               cq
               (hash-set cq))
        cf (cf/find-frame 'if)]
    (when-not cf (error "There is no frame associated with ~D=>." i))
    (when-not (and ant cq)
      (error i "=> must be given two arguments, ~
                         the antecedent(s) and the consequent(s), in ("i"=> "ant" "cq")."))
    (let [tot (count ant)]
      (when-not (<= 0 i tot)
	(error
	 "The number in "i"=> must be between 0
                and the number of antecedents, "(count ant)", in ("i"=> "ant" "cq")."))
      (cond
        ;; Canonical transformations
        (zero? i)
          (build (list 'and (build cq semtype)) semtype {} #{})
        :else 
          (let [term (build-channels
                       (build-molecular-node
                         cf (list (build ant semtype substitution #{}) (build cq semtype substitution #{}))
                         :csneps.core/Numericalentailment semtype
                         :fsemtype semtype :min i))]
            term)))))

;; Note, when a subrule name is gensymmed it can't be shared.
(defn subrule-helper
  "Helps build a subrule. If there is a string or symbol after :subrule, use it as the subrules name.
   Otherwise gensym a name."
  [parentname subrule subs]
  (let [name-or-term (second subrule)
        named? (or (symbol? name-or-term) (string? name-or-term))]
    (if named?
      (defrule-helper name-or-term (rest (rest subrule)) subs)
      (defrule-helper (gensym (str parentname "-subrule")) (rest subrule) subs))))

(defn build-rule 
  [rulename lhs forms subrules & {:keys [subs] :or {subs {}}}]
  ;(println rulename lhs forms subrules subs)
  (let [[built-lhs subs] (loop [lhs lhs
                                built-lhs #{}
                                subs subs]
                           (if (empty? lhs)
                             [built-lhs subs]
                             (let [[new-expr built-vars sub] (check-and-build-variables (first lhs) :additional-subs subs)]
                               (recur (rest lhs)
                                      (conj built-lhs (build new-expr :Proposition (set/union subs sub) #{}))
                                      (set/union subs sub)))))
        actfn (bound-fn [subst] 
                (when (= (count subs) (count (filter (fn [[k v]] (subst v)) subs)))
                  (when @verbose-rules
                    (send debug/screenprinter (fn [_] (println rulename "firing with substitutions:" (into {} (map (fn [[k v]] [k (:name (subst v))]) subs))))))
                  (eval-forms-with-locals (into {} (map (fn [[k v]] [k (:name (subst v))]) subs)) forms)))
        name (build rulename :Thing {} #{})
        act (build (str "act" (.hashCode forms)) :Action {} #{})
        subrules (set (map #(subrule-helper rulename % subs) subrules))]
    (doseq [v (vals subs)]
      (doseq [rst (seq (@restriction-set v))]
        (assert rst (ct/find-context 'OntologyCT)))
      (build-quantterm-channels v)
      ;(when (= (syntactic-type-of v) :csneps.core/Arbitrary) (lattice-insert v))
      )
    (let [cf (cf/find-frame 'rule)
          rule (build-channels (build-molecular-node cf (list name built-lhs act subrules) :csneps.core/CARule :Policy))]
      (dosync 
        (ref-set (:print-forms rule) (clojure.string/join "\n" forms))
        (alter csneps.core/primaction assoc act actfn))
      rule)))

(defn build-carule-channels
  "Channels are built from each generic term (proposition) to the 
   rule, and from the rule to the action, and any subrules."
  [carule]
  (let [slot-map (cf/dcsRelationTermsetMap carule)
        props (get slot-map (slot/find-slot 'condition))
        acts (get slot-map (slot/find-slot 'action))
        subrules (get slot-map (slot/find-slot 'subrule))]
    (doseq [p props :let [ch (build-channel p carule {} {})]] 
      (install-channel ch p carule :i-channel))
    (doseq [a acts :let [ch (build-channel carule a {} {})]] 
      (install-channel ch carule a :i-channel))
    (doseq [s subrules :let [ch (build-channel carule s {} {})]] 
      (install-channel ch carule s :i-channel))))

(defn build-quantterm-channels
  "Channels are built from each restriction, to the quantified term,
   and in each indefinite, from each dep."
  [quantterm]
  (if (variable? quantterm)
    (doseq [r (@restriction-set quantterm)
            :let [ch (build-channel r quantterm {} {})]]
      (install-channel ch r quantterm :i-channel))
    (doseq [d (@dependencies quantterm)
            :let [ch (build-channel d quantterm {} {})]]
      (install-channel ch d quantterm :g-channel))))

(defn build-unifier-channels
  "Channels built between unifiable terms."
  [unif]
  ;; No unifier channels should originate from a WhQuestion, or an analytic generic
  (when-not (and (@property-map (:source unif)) 
                 (or ((@property-map (:source unif)) :WhQuestion)
                     ((@property-map (:source unif)) :Analytic)))
    ;(println "building unif channel from " (:source unif) "to" (:target unif) "pmap" (@property-map (:source unif)))
    (let [s->t (build-channel (:source unif) (:target unif) (:sourcebind unif) (:targetbind unif))]
      (cond 
        (and (@property-map (:target unif)) ((@property-map (:target unif)) :Analytic))
        (install-channel s->t (:source unif) (:target unif) :i-channel)

        (or (not (@property-map (:source unif)))
            (and (@property-map (:source unif)) (not ((@property-map (:source unif)) :Analytic))))
        (install-channel s->t (:source unif) (:target unif) :i-channel)))))

(defn build-internal-channels
  [rnode ants cqs]
  ;; You have to build u-channels before i-channels, because of the following situation:
  ;; 1) p is asserted with forward inference.
  ;; 2) p->q is asserted (or defined)
  ;; 3) the p to p->q channel is built before the p->q to q channel, causing showproofs
  ;;    not to print the derivation, since it doesn't know about the consequent. 
  ;; The worst thing that can happen with this ordering is that backward-infer needs to be
  ;; continued by focused backward reasoning, which isn't terrible.
  ;; Build the u-channels
  (doseq [c cqs :let [ch (build-channel rnode c {} {})]]
    (install-channel ch rnode c :u-channel))
  ;; Build the i-channels
  (doseq [a ants :let [ch (build-channel a rnode {} {})]] 
    (install-channel ch a rnode :i-channel)))

(defn build-generic-channels
  [gnode ants]
  ;; Build the g-channels
  (doseq [a ants :let [ch (build-channel a gnode {} {})]]
    (when-not (and (arbitraryTerm? a) (not @(:fully-built a))) ;; Don't build var -> restriction channels (var won't be fully built yet).
      (install-channel ch a gnode :g-channel))))

(defn build-whquestion-channels
  [gnode ants]
  ;; Build the g-channels
  (doseq [a ants :let [ch (build-channel a gnode {} {})]]
    (when-not (and (queryTerm? a) (not @(:fully-built a))) ;; Don't build var -> restriction channels (var won't be fully built yet).
      (install-channel ch a gnode :g-channel))))

(defn build-channels
  [rnode]
  (let [slot-map (cf/dcsRelationTermsetMap rnode)]
    (case (type-of rnode)
      :csneps.core/CARule
      (build-carule-channels rnode)
      :csneps.core/Negation
      (build-internal-channels rnode (get slot-map (slot/find-slot 'nor)) (get slot-map (slot/find-slot 'nor)))
      :csneps.core/Conjunction
      (build-internal-channels rnode (get slot-map (slot/find-slot 'and)) (get slot-map (slot/find-slot 'and)))
      (:csneps.core/Andor 
       :csneps.core/Disjunction 
       :csneps.core/Xor
       :csneps.core/Nand)
      (build-internal-channels rnode (get slot-map (slot/find-slot 'andorargs)) (get slot-map (slot/find-slot 'andorargs)))
      (:csneps.core/Thresh
       :csneps.core/Equivalence)
      (build-internal-channels rnode (get slot-map (slot/find-slot 'threshargs)) (get slot-map (slot/find-slot 'threshargs)))
      (:csneps.core/Numericalentailment
       :csneps.core/Implication)
      (build-internal-channels rnode (get slot-map (slot/find-slot 'ant)) (get slot-map (slot/find-slot 'cq)))))
  rnode)

(defn build-user-term
  "Build a term for the expression expr, whose function is fcn,
       and whose contextual semantic type is to be semtype."
  [fn expr semtype substitution properties]
  ;(println "Building User Term... " fn expr semtype substitution)
  ;; fn = (first expr)
  (let [fcn (if-not (atom? fn) (build fn :Thing substitution #{}) fn)
        cf (or
             (cf/find-frame
               (condp = (type-of fcn)
                 clojure.lang.Symbol
                 (if (wftname? (str fcn))
                   (caseframe-for (get-term fcn))
                   fcn)
                 :csneps.core/Atom
                 (:name fcn)
                 :csneps.core/Molecular
                 (caseframe-for fcn)
                 (error
                   "The function \"symbol\", "fcn", is not an acceptable function \"symbol\".")))
             (and @KRNovice
                  (cf/defineNoviceCaseframe fcn expr)))]
    ;(println "Found CF: " cf)
    (when-not cf
      (error "There is no frame associated with "fcn"."))

    (let [fixed-expr (cond ;; Converts (fcn x y z) to (fcn #{x y z}) where necessary.
                       (and (cf/quotedpp? cf) 
                            (= (count (:slots cf)) 1)
                            (> (count (rest expr)) 1))
                       (list (first expr) (set (rest expr)))
                       (and (not (cf/quotedpp? cf))
                            (= (count (:slots cf)) 2)
                            (> (count (rest expr)) 1))
                       (list (first expr) (set (rest expr)))
                       :else expr)
          check (checkArity fcn fixed-expr cf)
          fillers (for [[arg rel] (map vector
                                       (if (cf/quotedpp? cf)
                                         (rest fixed-expr)
                                         fixed-expr)
                                       (:slots cf))]
                    (build arg (:type rel) substitution #{}))
          whfills (whquestion-fillers (set fillers))
          genfills (generic-fillers (set fillers))
          properties (set/union properties 
                                (cond
                                  (and (seq whfills) (seq genfills))
                                  #{:Generic :WhQuestion}
                                  (seq whfills)
                                  #{:WhQuestion}
                                  (seq genfills) 
                                  #{:Generic}))
          molnode (build-molecular-node cf fillers :csneps.core/Molecular semtype :properties properties)]
                                        ;(if (seq genfills)
                                          ;(if (subtypep semtype :Generic) semtype :Generic)
                                          ;semtype))]
      (when (seq genfills)
        (build-generic-channels molnode genfills))
      (when (seq whfills)
        (build-whquestion-channels molnode whfills))
      molnode)))

(defmulti build
  (fn [expr semtype substitution properties] [(type-of expr)]))

(defmethod build
;  "Returns an empty set."
  [nil] [expr semtype substitution properties]
  (hash-set))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name is expr
;       of the given semantic type,
;     and returns it."
  [java.lang.String] [expr semtype substitution properties]
  (build (symbol expr) semtype substitution properties))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name is expr
;       of the given semantic type,
;     and returns it."
  [java.lang.Character] [expr semtype substitution properties]
  (build (symbol (str expr)) semtype substitution properties))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the integer x
;       of the given semantic type,
;     and returns it."
  [java.lang.Integer] [expr semtype substitution properties]
  (build (symbol (str expr)) semtype substitution properties))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the long integer x
;       of the given semantic type,
;     and returns it."
  [java.lang.Long] [expr semtype substitution properties]
  (build (symbol (str expr)) semtype substitution properties))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the ratio x
;       of the given semantic type,
;     and returns it."
  [clojure.lang.Ratio] [expr semtype substitution properties]
  (build (symbol (str expr)) semtype substitution properties))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the floating point number x,
;           rounded to *PRECISION* digits
;       of the given semantic type,
;     and returns it."
  [java.lang.Double] [expr semtype substitution properties]
  (build (symbol (str (roundf expr))) semtype substitution properties))

(defmethod build
;  "Returns the given term,
;       and if necessary, adjusting its semantic type so that
;       it is of the semantic type semtype."
  [:csneps.core/Term] [expr semtype substitution properties]
  (adjust-type expr (st/semantic-type-of expr) semtype))

(defmethod build
  [clojure.lang.Symbol] [expr semtype substitution properties]
  ;(println "Building symbol: " expr)
  (let [term (or 
               (and (seq substitution)
                    (substitution expr))
               (get-term expr))]
    (cond
      (or (wftname? (str expr))
          (quantterm? (str expr)))
        (if term ;;Lower it's semantic type if necessary
          (adjust-type term (st/semantic-type-of term) semtype)
          (error (str "The name " expr " is not yet associated with a term.")))
      term ;Lower its semantic type, if necessary
      (adjust-type term (st/semantic-type-of term) semtype)
      :else
        (let [term (new-atom {:name expr})]
          (dosync 
            (alter TERMS assoc expr term)
            (set-term-type term semtype)
            (when (subtypep semtype :Proposition) (alter support assoc term #{['hyp #{(:name term)}]}))
            (alter msgs assoc term (create-message-structure :csneps.core/Atom nil)))
          (when (= expr 'True)
            (assert term (ct/find-context 'OntologyCT)))
          (when (= expr 'False)
            (assert 
              (build (list 'not term) :Proposition substitution)
              (ct/find-context 'OntologyCT)))
          term))))


(defmethod build
  [clojure.lang.LazySeq] [expr semtype substitution properties]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution properties))

(defmethod build
  [clojure.lang.PersistentList] [expr semtype substitution properties]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution properties))

(defmethod build
  [clojure.lang.Cons] [expr semtype substitution properties]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution properties))

(defmethod build
  [clojure.lang.PersistentVector$ChunkedSeq] [expr semtype substitution properties]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution properties))

(defmethod build
  [clojure.lang.PersistentHashSet] [expr semtype substitution properties] 
  (let [set (set (map #(build % semtype substitution properties) expr))]
    (if (= (count set) 1) (first set) set)))

(defmethod build
;    "Creates (if necessary) a well-formed term expressed by expr
;       of the given semantic type,
;     and returns it."
  [clojure.lang.PersistentVector] [expr semtype substitution properties]
  (let [fcn (first expr)]
    ;(println "Building: " expr semtype substitution)
    ;(println "Subs:" substitution)
    (case fcn
      ;; Only functions with special syntax
      ;;    or which build terms of a special syntactic type
      ;;    need to be handeled specially.
      ;; All others can use the default case of buildUserTerm.
      Isa 
        (do
          (clojure.core/assert (= (count expr) 3)
                               (str "Isa must take 2 arguments. It doesn't in " expr "."))
          (let [entity (build (second expr) :Entity substitution #{})
                category (build (third expr) :Category substitution #{})
                genfills (if (= entity category)
                           (generic-fillers #{entity})
                           (generic-fillers #{entity category}))
                whfills (if (= entity category)
                          (whquestion-fillers #{entity})
                          (whquestion-fillers #{entity category}))
                properties (set/union properties 
                                  (cond
                                    (and (seq whfills) (seq genfills))
                                    #{:Generic :WhQuestion}
                                    (seq whfills)
                                    #{:WhQuestion}
                                    (seq genfills) 
                                    #{:Generic}))
                molnode (build-molecular-node (cf/find-frame 'Isa)
                                              (list entity category)
                                              :csneps.core/Categorization
                                              semtype
                                              :properties properties)]
            (when (seq genfills)
              (build-generic-channels molnode genfills))
            (when (seq whfills)
              (build-whquestion-channels molnode whfills))
            molnode))
              
      and
        (if-let [cf (cf/find-frame 'and)]
          (cond
            (rest (rest expr)) ;;>1 conjunct
              (let [fillers (build (set (rest expr)) semtype substitution #{})]
                (build-channels (build-molecular-node cf 
                                                      (list fillers) 
                                                      :csneps.core/Conjunction 
                                                      semtype 
                                                      :fsemtype (st/semantic-type-of fillers)
                                                      :min (count fillers)
                                                      :max (count fillers))))
            (rest expr) ;;1 conjunct
               (if (or (and (cl-seqable? (second expr)) (= (first (second expr)) 'setof))
                       (set? (second expr)))
                ;; if the argument is a set, treat it as the set of conjuncts
                (if (= (count (second expr)) 1)
                  (first (second expr))
                  (build-channels (build-molecular-node cf 
                                                        (rest expr) 
                                                        :csneps.core/Conjunction 
                                                        semtype 
                                                        :fsemtype 
                                                        (st/semantic-type-of (second expr))
                                                        :min (count (rest expr))
                                                        :max (count (rest expr)))))
                ;; otherwise, just build the conjunct.
                (build (second expr) semtype substitution #{}))
            :else
              (build 'True :csneps.core/Proposition substitution #{}))
          (error "There is no frame associated with and."))

      or
      ;; expr is (or a1 ... an)
      (if-let [cf (cf/find-frame 'andor)]
        (cond
          (rest (rest expr))
          (let [fillers (build (set (rest expr)) semtype substitution #{})]
            (build-channels 
              (build-molecular-node cf (list fillers) :csneps.core/Disjunction semtype :fsemtype (st/semantic-type-of fillers)
                                    :min 1 :max (count fillers))))
          (rest expr)
          ;; If only one disjunct, just build the disjunct.
          (build (second expr) semtype substitution #{})
          :else
          ;; A disjunction with no disjuncts is the False proposition
          (build 'False :csneps.core/Term substitution #{}))
        (error "There is no frame associated with or"))

      xor
      ;; expr is (xor a1 ... an)
      (if-let [cf (cf/find-frame 'andor)]
        (cond
          (rest (rest expr))
          (let [fillers (build (set (rest expr)) semtype substitution #{})]
            (build-channels 
              (build-molecular-node cf (list fillers) :csneps.core/Xor semtype :fsemtype (st/semantic-type-of fillers)
                                    :min 1 :max 1)))
          (rest expr)
          ;; If only one disjunct, just build the disjunct.
          (build (second expr) semtype substitution #{})
          :else
          ;; An exclusive disjunction with no disjuncts is the False proposition
          (build 'False :csneps.core/Term substitution #{}))
        (error "There is no frame associated with xor"))
      
      nand
       ;; expr is (nand a1 ... an)
       (if-let [cf (cf/find-frame 'andor)]
         (cond
           (or 
             (> (count expr) 2)
             (and (set? (second expr))
                  (> (count (second expr)) 1)))
           (let [fillers (build (set (rest expr)) semtype substitution #{})]
             (build-channels 
               (build-molecular-node cf (list fillers) :csneps.core/Nand semtype :fsemtype (st/semantic-type-of fillers)
                                     :min 0 :max (dec (count fillers)))))
           (= (count expr) 2)
           ;; If only one argument, build the negation of the argument.
           (build (list 'not (second expr)) semtype substitution #{})
           :else
           ;; A negatedconjunction with no arguments is the False proposition
           (build 'False :csneps.core/Term substitution #{}))
         (error "There is no frame associated with nand"))

      andor
       ;; expr is (andor (i j) a1 ... an)
      (build-andor (rest expr) semtype substitution)

      thresh
       ;; expr is  (thresh (i j) a1 ... an)
      (build-thresh (rest expr) semtype substitution)
      
      if
        ;; expr is (if a1 a2)
      (if-let [cf (cf/find-frame 'if)]
        (do
          (checkArity 'if expr cf)
          (let [fillers1 (build (second expr) semtype substitution #{})
                fillers2 (build (nth expr 2) semtype substitution #{})]
            (build-channels 
              (build-molecular-node
                cf (list fillers1 fillers2) :csneps.core/Implication semtype
                :fsemtype semtype :min (if (set? fillers1)
                                         (count fillers1)
                                         1)))))
        (error "There is no frame associated with if"))

      iff
       ;; expr is (iff a1 ... an)
      (if-let [cf (cf/find-frame 'thresh)]
        (cond
          (rest (rest expr))
          (let [fillers (build (set (rest expr)) semtype substitution #{})]
            (build-channels
              (build-molecular-node
                cf (list fillers) :csneps.core/Equivalence semtype
                :fsemtype (st/semantic-type-of fillers)
                :min 1 :max (dec (count fillers)))))
          (rest expr)
          ;; If only one filler,
          (if (set? (second expr))
            ;; if the argument is a set, treat it as the set of fillers
            (if (= (count (second expr)) 1)
              ;; If just one filler, its a tautology
              (first (second expr))
              ;; otherwise, build the iff
              (build-channels 
                (build-molecular-node
                  cf (rest expr) :csneps.core/Equivalence semtype
                  :fsemtype (st/semantic-type-of (second expr))
                  :min 1 :max (dec (count (first (rest expr)))))))
            (build 'True :csneps.core/Term substitution #{}))
          :else
          ;; A iff with no fillers is the True proposition
          (build 'True :csneps.core/Term substitution #{}))
        (error "There is no frame associated with iff."))

       (not nor)
       ;; expr is (nor a1 ... an) or (not a1 ... an)
       (if-let [cf (cf/find-frame 'nor)]
         (cond
           ;(third expr)		; at least two arguments
           (rest expr) ;;1 or more args
           (let [fillers (build (set (rest expr)) semtype substitution #{})]
             (build-channels
               (build-molecular-node
                 cf (list fillers) :csneps.core/Negation semtype
                 :fsemtype (st/semantic-type-of fillers))))
           ;(rest expr)		; exactly one argument
           ;  (build-canonical-negation (second expr) semtype)
           :else			; (not) = (nor) = T
           (build 'True :csneps.core/Term substitution #{}))
         (error "There is no frame associated with nor."))

      (thnot thnor)
      ;; expr is (thnor a1 ... an) or (thnot a1 ... an)
      (if-let [cf (cf/find-frame 'thnor)]
        (cond
          (rest expr)		; at least one argument
          (let [fillers (build (set (rest expr)) semtype substitution #{})]
            (build-molecular-node
              cf (list fillers) :csneps.core/Negationbyfailure semtype
              :fsemtype (st/semantic-type-of fillers)))
          :else			; (thnot) = (thnor) = T
          (build 'True :csneps.core/Term substitution #{}))
        (error "There is no frame associated with thnor."))


      setof
      (let [set (set (for [arg (rest expr)] (build arg semtype substitution #{})))]
        (if (= (count set) 1) (first set) set))
      
      close
      (let [closed-var-labels (if (cl-seqable? (second expr)) (second expr) (list (second expr)))
            closed-vars (map substitution closed-var-labels)
            fillers (build (set (rest (rest expr))) :Proposition substitution #{})]
        (build-molecular-node (cf/find-frame 'close)
	                            (list fillers)
	                            :csneps.core/Closure
	                            semtype
                              :closed-vars closed-vars))

      rule 
      (let [rulename (second expr)
            lhs (nth expr 2)
            forms (nth expr 3)
            subrules (last expr)]
        (build-rule rulename lhs forms subrules :subs substitution))
      
      (cond ;;Else caseframes
        (ientailsymb? fcn)
            ;; expr is (i=> ant cq)
            (build-numerical-entailmant (ientaili fcn) (second expr) (third expr) semtype substitution)
            :else
            (build-user-term fcn expr semtype substitution properties)))))

(defn pre-build-var
  "This function creates a variable without a restriciton set. This is
   needed to handle cases of mutual dependencies. quant indicates the
   type of variable and is either the symbol 'some or
   'every. var-label is the label the user used to denote this
   variable. rsts are the restriction set specifier for the variable,
   but are not actually constructed here. Simple error checking is
   done at this stage, as is a search of the KB for pre-existing
   arbitrary nodes with this restriction set. The substitution is a mapping
   between these labels and the
   variables they build. This function returns the varibale node built
   and modifies the substitution accordingly. Arbitrary individuals may
   have a resriction set slot filled in already if they existed in the KB
   previously. Otherwise, resrtiction sets still need to be built."
  [quant var-label rsts notsames & {:keys [arb-rsts ind-rsts qvar-rsts reuse-inds]}]
  (doseq [rst rsts
            :when (not (some #{var-label} (flatten (prewalk (fn [x] 
                                                              (if (set? x) 
                                                                (seq x) 
                                                                x)) 
                                                            rst))))]
    (error
      (str "The variable label, " var-label ", is not part of the restriction proposition, " rst ".")))

  (or 
    (and 
      (or (= quant :qvar) (= quant :every)) 
      (find/find-old-var-node var-label rsts arb-rsts ind-rsts qvar-rsts quant notsames))
    (and reuse-inds
         (= quant :some)
         (find/find-old-var-node var-label rsts arb-rsts ind-rsts qvar-rsts quant notsames))
    (let [name (case quant
                 :every (symbol (str "arb" (arb-counter)))
                 :some (symbol (str "ind" (ind-counter)))
                 :qvar (symbol (str "qvar" (qvar-counter))))
          varterm (case quant
                    :every (new-arbitrary {:name name 
                                           :var-label var-label})
                    :some (new-indefinite {:name name 
                                           :var-label var-label})
                    :qvar (new-query-variable {:name name 
                                               :var-label var-label}))]

       (case quant
         :every (inc-arb-counter)
         :some  (inc-ind-counter)
         :qvar  (inc-qvar-counter))
       (st/instantiate-sem-type varterm :Entity)
        
      varterm)))


(defn pre-build-vars
  "Loops through the arbitrary individuals and builds an initial
   structure, but not the restriction sets. The same is done for the
   indefinite objects."
  [arb-rsts ind-dep-rsts qvar-rsts notsames & {:keys [reuse-inds]}]
  (into {}
        (concat (for [k (seq (keys arb-rsts))]
                  [k (pre-build-var :every k (get arb-rsts k) notsames :arb-rsts arb-rsts :ind-rsts ind-dep-rsts :qvar-rsts qvar-rsts :reuse-inds reuse-inds)])
                (for [k (seq (keys ind-dep-rsts))]
                  [k (pre-build-var :some k (second (get ind-dep-rsts k)) notsames :arb-rsts arb-rsts :ind-rsts ind-dep-rsts :qvar-rsts qvar-rsts :reuse-inds reuse-inds)])
                (for [k (seq (keys qvar-rsts))]
                  [k (pre-build-var :qvar k (get qvar-rsts k) notsames :arb-rsts arb-rsts :ind-rsts ind-dep-rsts :qvar-rsts qvar-rsts :reuse-inds reuse-inds)]))))

(defn internal-restrict
  [var]
  (let [res (@restriction-set var)
        categorizations (filter #(isa? (syntactic-type-of %) :csneps.core/Categorization) res)
        categories (apply clojure.set/union (map #(second (@down-cableset %)) categorizations))
        semcats (filter semtype? (map #(keyword (:name %)) categories))]
    (doall (map #(adjust-type var (st/semantic-type-of var) %) semcats))))

;;; Note: Doesn't deal with if the user puts >1 notSame relation.
(defn- notsames-helper 
  "Finds a (notSame ...) relation, and builds a set of varlabels this term is not
   the same as. Returns that set, and the set of restrictions without the
   notSame relation."
  [rsts var-label]
  (let [[rsts not-same-var-labels] (loop [rsts rsts
                                          seen '()]
                                     (cond
                                       (empty? rsts)
                                       [seen #{}]
                                       (= (ffirst rsts) 'notSame)
                                       [(into (rest rsts) seen) (rest (first rsts))]
                                       :else
                                       (recur (rest rsts)
                                              (conj seen (first rsts)))))
        not-same-var-labels (disj (set not-same-var-labels) var-label)]
    [rsts not-same-var-labels]))

(defn- notsames 
  [arb-rsts qvar-rsts ind-dep-rsts]
  (let [arb (for [[k v] arb-rsts :let [[new-rsts nsvarlabels] (notsames-helper v k)]]
              [[k new-rsts] [k nsvarlabels]])
        arb-rsts (into {} (map #(first %) arb))
        notsame (into {} (map #(second %) arb))
        qvar (for [[k v] qvar-rsts :let [[new-rsts nsvarlabels] (notsames-helper v k)]]
              [[k new-rsts] [k nsvarlabels]])
        qvar-rsts (into {} (map #(first %) qvar))
        notsame (into notsame (map #(second %) qvar))
        ind (for [[k v] ind-dep-rsts :let [[new-rsts nsvarlabels] (notsames-helper (second v) k)]]
             [[k (list (first v) new-rsts)] [k nsvarlabels]])
        ind-dep-rsts (into {} (map #(first %) ind))
        notsame (into notsame (map #(second %) ind))]
    [arb-rsts qvar-rsts ind-dep-rsts notsame]))

(defn generate-notsames
  "If the user names two arbs/qvars apart in one expr, assume they mean different (notsame) ones."
  [rsts]
  (loop [rsts rsts
         rsts-done []]
    (if (empty? rsts)
      (into {} rsts-done)
      ;; Determine if the first restiction is the same as any other in the sequence,
      ;; modulo only the variable label used.
      (let [replaced-variable (map #(vector 
                                      (first %) 
                                      (prewalk-replace {(first %) (ffirst rsts)} (second %))
                                      (second %)) 
                                   (rest rsts))
            equal-restrictions (filter #(= (second (first rsts))
                                           (second %))
                                       replaced-variable)
            equal-restriction-labels (map first equal-restrictions)
            remaining (filter #(not ((set equal-restriction-labels) (first %))) (rest rsts))
            finished (conj (map #(vector (first %) (third %)) equal-restrictions) (first rsts) )
            finished (loop [nsvars [(ffirst finished)]
                            remrsts (rest finished)
                            fixedrsts [(first finished)]]
                       (if (empty? remrsts)
                         fixedrsts
                         (recur 
                           (conj nsvars (ffirst remrsts))
                           (rest remrsts)
                           (conj fixedrsts [(ffirst remrsts) 
                                            (conj (second (first remrsts)) 
                                                  `[~'notSame ~@nsvars ~(ffirst remrsts)])]))))]
        (recur
          remaining
          (into rsts-done finished))))))

(defn build-var
  "Build a variable node of type quant with label var-label and
   restrictions rsts. Substitution contains all the variable nodes
   that should be needed to build the restrictions. Dependencies is only
   needed for building indefinite objects."
  [quant var-label rsts substitution notsames & {:keys [deps]}]
  (let [var (substitution var-label)
        nsvars (set (map #(let [nsvar (or (substitution %)
                                           (get-term %))] ;; Allows terms to be used in notsames.
                            (when-not nsvar
                              (error (str "Cannot build notSame for variable " % " which is out of scope.")))
                            nsvar)
                         (notsames var-label)))]
    (when-not (@restriction-set var) ;; already built!
      (alter TERMS assoc (:name var) var)
      (when (some nil? nsvars) (error "Cannot build notSame for variable out of scope."))
      (alter (:not-same-as var) clojure.set/union nsvars)
      (cond 
        (= quant :qvar) 
        (let [restrictions (clojure.set/union (@restriction-set var)
                                              (set (map #(build % :Propositional substitution #{:WhQuestion :Analytic}) rsts)))]
          (alter restriction-set assoc var restrictions)
          (alter msgs assoc var (create-message-structure :csneps.core/QueryVariable nil))
          ;(dosync (doseq [r restrictions]
          ;          (alter property-map assoc r (set/union (@property-map r) #{:WhQuestion :Analytic}))))
          )
        (and (= quant :some) (not (nil? dependencies)))
        (let [restrictions (set (map #(build % :Propositional substitution #{:Generic :Analytic}) rsts))
              deps (clojure.set/union (@dependencies var)
                                      (set (map #(substitution %) deps)))] 
          (alter dependencies assoc var deps)
          (alter restriction-set assoc var restrictions)
          (alter msgs assoc var (create-message-structure :csneps.core/Indefinite nil))
          ;(dosync (doseq [r restrictions]
          ;          (alter property-map assoc r (set/union (@property-map r) #{:Generic :Analytic}))))
          )
        :else
        (let [restrictions (clojure.set/union (@restriction-set var)
                                              (set (map #(build % :Propositional substitution #{:Generic :Analytic}) rsts)))] 
          (alter restriction-set assoc var restrictions)
          (alter msgs assoc var (create-message-structure :csneps.core/Arbitrary restrictions))
          ;(dosync (doseq [r restrictions]
          ;          (alter property-map assoc r (set/union (@property-map r) #{:Generic :Analytic}))))
          ))
    
       (internal-restrict var)
      
       (alter TERMS assoc (:name var) var)
       (case quant
         :every (do 
                  (alter ARBITRARIES conj var)
                  (ref-set (:fully-built var) true))
         :some  (alter INDEFINITES conj var)
         :qvar  (do 
                  (alter QVARS conj var)
                  (ref-set (:fully-built var) true))))
    
    var))

(defn build-vars
  "Loops through the arbs and inds. and builds their restriction
   sets. In the case of indenfinite-objects, their dependency lists
   are populated."
  [arb-rsts ind-dep-rsts qvar-rsts substitution notsames]
  (set 
    (concat
      (for [k (keys arb-rsts)]
        (build-var :every k (get arb-rsts k) substitution notsames))
      (for [k (keys ind-dep-rsts)]
        (build-var :some k (second (get ind-dep-rsts k)) substitution notsames
                   :deps (first (get ind-dep-rsts k))))
      (for [k (keys qvar-rsts)]
        (build-var :qvar k (get qvar-rsts k) substitution notsames)))))

(defn function-symbol?
  "Checks if a literal is a function symbol." 
  [literal]
  (@cf/FN2CF literal))
  
(defn expand-rst
  [var rst]
  (cond
    (function-symbol? rst) [rst var]
    (or (symbol? rst) (string? rst)) ['Isa var rst]
    (and (cl-seqable? rst)
         (not (some #(= % var) rst))
         (not= (count (rest rst)) (count (:slots (cf/find-frame (first rst))))))
    (into [(first rst) var] (rest rst))
    :else rst))

(defn expand-rsts
  "Expands rsts to account for the syntactic sugar allowed in writing them."
  [rsts]
  ;; rsts are a map from var name to restrictions. 
  (let [var-rsts-pairs (vec rsts)]
    (into {}
          (for [[var rsts] var-rsts-pairs]
            (if (empty? rsts)
              [var [(list 'Isa var 'Entity)]]
              [var (mapv #(expand-rst var %) rsts)])))))

(defn expand-ind-dep-rsts
  [rsts]
  (let [var-rsts-pairs (vec rsts)]
    (into {}
          (for [[var dep-rsts] var-rsts-pairs
                :let [[dep rsts] dep-rsts]]
            (if (empty? rsts)
              [var (list dep [(list 'Isa var 'Entity)])]
              [var (list dep (mapv #(expand-rst var %) rsts))])))))

(defn- merge-error
  [fir lat]
  (when (not= fir lat)
    (error "Duplicate variable label use!"))
  fir)

(defn parse-vars-and-rsts
   "Helper function: assertion-spec is a specifier for building a proposition
   in SNePS 3 (e.g., '(Isa (every x (Dog x) (Black x)) Mammal)). This
   function loops through assertion-spec and returns the expression
   with variable specifiers replaced with the variable label
   (e.g. (Isa x Mammal)). This function also takes in two empty hash-tables,
   arb-rsts and ind-deps-rsts. These are modified to contain a mapping
   between var-labels and their restriciton sets and given dependencies
   if they are indefinite objects. For example the proposition
   (Beats (every x (Isa x Farmer)
                   (Owns x (some y(x) (Isa y Donkey))))
          y)
   results in:

  Ex. ind-deps-rsts: [y -> ((x) (Isa y Donkey))]
      arb-rsts:  [x ->  ((Isa x Farmer) (Owns x y))]"
  [assertion-spec arb-rsts ind-deps-rsts qvar-rsts & {:keys [buildingqvar] :or {buildingqvar #{}}}]
  (cond
    (and (cl-seqable? assertion-spec) (not (set? assertion-spec)) (not (map? assertion-spec)))
    (cond
      (= (first assertion-spec) 'some)
      (let [rsts (rest (ind-deps-rsts (second assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference (set rsts) 
                                                 (set (rest (rest (rest assertion-spec)))))))
          (error (str "Restriction sets: " rsts " and " (rest (rest assertion-spec)) " cannot be applied to
                      the same variable."))
          :else 
          (let [[aspec ar idr qvr] (parse-vars-and-rsts (rest (rest (rest assertion-spec)))
                                                        arb-rsts ind-deps-rsts qvar-rsts
                                                        :buildingqvar buildingqvar)
                idr (assoc idr
                           (second assertion-spec) 
                           (list (nth assertion-spec 2) aspec))]
            [(second assertion-spec) ar idr qvr])))
      (#{'every 'any} (first assertion-spec))
      (let [rsts (arb-rsts (second assertion-spec))]
        (cond
          (and rsts (not (empty? (clojure.set/difference (set rsts)
                                                         (set (map vec (rest (rest assertion-spec))))))))
          (error (str "Restriction sets: " rsts " and " (rest (rest assertion-spec)) " cannot be applied to
                      the same variable."))
          :else
          (let [[aspec ar idr qvr] (parse-vars-and-rsts (rest (rest assertion-spec))
                                                        arb-rsts ind-deps-rsts qvar-rsts
                                                        :buildingqvar buildingqvar)
                ar (assoc ar (second assertion-spec) aspec)]
            [(second assertion-spec) ar idr qvr])))
      (synvariable? (first assertion-spec))
      (let [rsts (second (qvar-rsts (first assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference (set rsts) 
                                                 (set (rest assertion-spec)))))
          (error (str "Restriction sets: " rsts " and " (rest assertion-spec) " cannot be applied to
                      the same variable."))
          :else
          (let [[aspec ar idr qvr] (parse-vars-and-rsts (rest assertion-spec) 
                                                        arb-rsts ind-deps-rsts qvar-rsts
                                                        :buildingqvar (conj buildingqvar (first assertion-spec)))
                qvr (assoc qvar-rsts (first assertion-spec) aspec)]
                [(first assertion-spec) ar idr qvr])))
      :else
      (loop [assertion-spec assertion-spec
             new-expr []
             arb-rsts arb-rsts
             ind-deps-rsts ind-deps-rsts
             qvar-rsts qvar-rsts]
        (if (empty? assertion-spec)
          [new-expr arb-rsts ind-deps-rsts qvar-rsts]
          (let [[aspec ar idr qvr] (parse-vars-and-rsts (first assertion-spec) arb-rsts ind-deps-rsts qvar-rsts :buildingqvar buildingqvar)]
            (recur
              (rest assertion-spec)
              (conj new-expr aspec)
              (merge-with merge-error arb-rsts ar)
              (merge-with merge-error ind-deps-rsts idr)
              (merge-with merge-error qvar-rsts qvr))))))
    (set? assertion-spec)
    (loop [assertion-spec assertion-spec
             new-expr #{}
             arb-rsts arb-rsts
             ind-deps-rsts ind-deps-rsts
             qvar-rsts qvar-rsts]
        (if (empty? assertion-spec)
          [new-expr arb-rsts ind-deps-rsts qvar-rsts]
          (let [[aspec ar idr qvr] (parse-vars-and-rsts (first assertion-spec) arb-rsts ind-deps-rsts qvar-rsts :buildingqvar buildingqvar)]
            (recur
              (rest assertion-spec)
              (conj new-expr aspec)
              (merge-with merge-error arb-rsts ar)
              (merge-with merge-error ind-deps-rsts idr)
              (merge-with merge-error qvar-rsts qvr)))))
    (and (synvariable? assertion-spec)
         (not (buildingqvar assertion-spec))) ;; Don't add the Entity restriction later! 
    (let [qvar-rsts (assoc qvar-rsts assertion-spec (list (list 'Isa assertion-spec 'Entity)))]
      [assertion-spec arb-rsts ind-deps-rsts qvar-rsts])
    :else 
    [assertion-spec arb-rsts ind-deps-rsts qvar-rsts]))

;; Additional-subs allows a way to bring other variables that would be out of scope into this scope. This is used in
;; condition-action rules, especially in subrule handling.
(defn check-and-build-variables
  "Given a top-level build expression, checks that expression for
   variable terms syntax (e.g., every, some). These terms are built and
   a new build expression is created with just the variable-labels
   (e.g., x in (every x ...)). This and a substitution between
   variable-labels and the AI and IO is provided to build.
   Returns three values. The first is the new build expression, the
   second the the built nodes, and the third the substitution between
   var-labels and the AI/IO nodes."
  [expr & {:keys [reuse-inds additional-subs] :or {additional-subs {}}}]
  (dosync
    (let [[new-expr arb-rsts ind-dep-rsts qvar-rsts] (parse-vars-and-rsts expr {} {} {})
          arb-rsts (generate-notsames (expand-rsts arb-rsts))
          qvar-rsts (generate-notsames (expand-rsts qvar-rsts))
          ind-dep-rsts (expand-ind-dep-rsts ind-dep-rsts)
          [arb-rsts qvar-rsts ind-dep-rsts notsames] (notsames arb-rsts qvar-rsts ind-dep-rsts)
          substitution (merge (pre-build-vars arb-rsts ind-dep-rsts qvar-rsts notsames :reuse-inds reuse-inds) additional-subs)
          built-vars (build-vars arb-rsts ind-dep-rsts qvar-rsts substitution notsames)]
      [new-expr built-vars substitution])))
