(ns csneps.core.build
  (:require [clojure.math.combinatorics :as cb])
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.printer :as print]
            [csneps.core.relations :as slot]
            [clojure.core.match :as match]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as cb]
            [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.string])
  ;(:refer-clojure :exclude [assert]) ;;Possible bug: This breaks loading clojure.math.combinatorics for no reason?
  (:use [csneps.core]
        [csneps.util]
        [clojure.walk :as walk :only [prewalk prewalk-replace]]
        [csneps.core.find-utils]))

;(refer-clojure :exclude '[assert])

(declare assert build check-and-build-variables build-channels build-unifier-channels build-quantterm-channels create-message-structure generic-term? check-and-build-variables)

(load "build_assert")
(load "build_utils")
(load "build_substitution")
(load "build_subsumption")
(load "build_unification")
(load "build_find")
(load "build_channel")
(load "build_rules")
(load "build_rewrite")

(defvar KRNovice (ref nil)
  "If the value is non-null,
      caseframes will be created automatically
        whenever the user uses a function symbol
            that doesn't already have a caseframe.")

(defvar ^:dynamic *PRECISION* 5)

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

(defn filler-generic?
  "The filler of a slot is generic if:
     1) It is a term with semantic type Generic, or
     2) It is a term with syntactic type Arbitrary, or
     3) It is a set of terms in which any one term satisfies 1 or 2."
  [term-or-termset] 
  (let [gt? (fn [term] (or (generic-term? term) (isa? (type-of term) :csneps.core/Arbitrary)))]
    (if (set? term-or-termset)
      (some gt? term-or-termset)
      (gt? term-or-termset))))

(defn fillers-generic?
  "Given a list of fillers for slots, if any one of them satisfies the
     conditions of filler-generic?, then we say they all do."
  [fillers]
  (some filler-generic? fillers))

(defn generic-fillers
  "Returns the generic fillers in term-or-termset"
  [term-or-termset]
  (let [gt (fn [term] (when (or (generic-term? term) (isa? (type-of term) :csneps.core/Arbitrary))
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

(defn adjustType
  "Adjusts the type of term, if possible,
       from its old semantic type, oldtype,
       to its new semantic type, newtype,
       while keeping its syntactic type, syntype,
    and return the term."
  [term oldtype newtype]
  ;(println "Adjusting type of: " (:name term) " from: " oldtype " -> " newtype)
  (cond
    ;; Types are already the same
    (= (type-of term) (type-of newtype)) nil
    ;; Arbitrary terms can be adjusted down only while they're being built.
    ;; They can't ever be adjusted down below their highest-level restriction.
    (and (or (arbitraryTerm? term) (queryTerm? term))
         @(:fully-built term)
         (not (subtypep oldtype newtype)))
    (error "Cannot adjust an arbitrary term " (:name term) " from type " oldtype " to more specific type " newtype ".")
    ;; Newtype is a subtype of oldtype
    (subtypep newtype oldtype)
    (dosync (alter type-map assoc (:name term) newtype))
    ;; If new type and oldtype have a common subtype,
    ;;    change the type to be the greatest common subtype
    ;; Could there be more than 1 greatest common subtype?
    :else
    (if-let [gcsub (gcsubtype newtype oldtype)]
      (if (> (count gcsub) 1)
        (let [gcsubchoice (menuChooseFromList (str "Choose a type for " term ".") gcsub)]
          (if gcsubchoice
            (dosync (alter type-map assoc (:name term) gcsubchoice))
            (error "No semantic type for " term ".")))
        (dosync (alter type-map assoc (:name term) (first gcsub))))
      (error (str "Type Error: Cannot adjust " (:name term) " from " oldtype " to " newtype "."))))
  ;; Propositions are true in contexts where they are hyps.
  (when (or 
          (and (subtypep newtype :Proposition) (not (subtypep oldtype :Proposition)))
          (and (nil? (@support term)) (subtypep newtype :Policy)))
    (dosync (alter support assoc term #{['hyp #{(:name term)}]})))
  term)

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
  [cf dcs syntype semtype & {:keys [fsemtype min max closed-vars]}]
  ;(println "building molecular..." dcs (doall (map make-set-if-not-set dcs)))
  (let [dcs-sets (map make-set-if-not-set dcs)
        tests (doall (map check-min-max dcs-sets (:slots cf)))
        existing-term (cond 
                        max (find-exact syntype cf dcs-sets :min min :max max)
                        min (find-exact syntype cf dcs-sets :min min)
                        :else (find-exact syntype cf dcs-sets))
        term (or
               existing-term
               (let [wft ((get-new-syntype-fn syntype) {:name (symbol (str "wft" (inc-wft-counter)))
                                                        :min (if (nil? min) 0 min)
                                                        :max (if (nil? max) 0 max)
                                                        :closed-vars closed-vars})]
                 (dosync 
                   (alter down-cableset assoc wft dcs-sets)
                   (alter caseframe assoc wft cf)
                   (alter msgs assoc wft (create-message-structure syntype dcs-sets :n min))
                   (alter TERMS assoc (:name wft) wft)
                   (alter type-map assoc (:name wft) (:type cf)))
                 
                 (initialize-syntype wft)
                 
                 (cf/add-caseframe-term wft :cf cf)
                 wft))]

    (adjustType term (or (@type-map (:name term)) (:type cf)) (if fsemtype fsemtype semtype))
    
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
  [args semtype]
  (let [cf (cf/find-frame 'andor)]
    (when-not cf (error "There is no frame associated with andor."))
    (when-not (seqable? (first args))
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
          (build 'True semtype {})
        (= tot min max)
          (build (list* 'and (rest args)) semtype {})
        (= 0 min max)
          (build (list* 'nor (rest args)) semtype {})
        (and (zero? min) (= max (dec tot)))
          (build (list* 'nand (rest args)) semtype {})
        :else
          (let [fillers (build (list* 'setof (rest args)) semtype {})
                term (build-molecular-node
                       cf (list fillers) :csneps.core/Andor semtype
                       :fsemtype (semantic-type-of fillers)
                       :min min :max max)]
            (build-channels term)
            term)))))

(defn build-thresh
  "Build a term for thresh.
       args is the original expression after 'thresh'."
  [args semtype]
  (let [cf (cf/find-frame 'thresh)]
    (when-not cf (error "There is no frame associated with thresh."))
    (when-not (seqable? (first args))
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
          (build 'False semtype {})
        (and (zero? min) (= max (dec tot)))
          (build (list* 'and (rest args)) semtype {}) 
        (= min max 0)
          (build (list* 'or (rest args)) semtype {})
        (and (= min 1) (= max tot))
          (build (list* 'nor (rest args)) semtype {})
        (= min tot)
          (build (list* 'nand (rest args)) semtype {})
        :else 
          (let [fillers (build (list* 'setof (rest args)) semtype {})
                term (build-channels 
                       (build-molecular-node
                         cf (list fillers) :csneps.core/Thresh semtype
                         :fsemtype (semantic-type-of fillers)
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
          (build (list 'and (build cq semtype)) semtype {})
        :else 
          (let [term (build-channels
                       (build-molecular-node
                         cf (list (build ant semtype substitution) (build cq semtype substitution))
                         :csneps.core/Numericalentailment semtype
                         :fsemtype semtype :min i))]
            term)))))

(defn build-rule 
  [rulename lhs forms subrules & {:keys [subs] :or {subs {}}}]
  ;(println rulename lhs forms subrules subs)
  (let [[built-lhs subs] (loop [lhs lhs
                                built-lhs #{}
                                subs subs]
                           (if (empty? lhs)
                             [built-lhs subs]
                             (let [[new-expr built-vars sub] (check-and-build-variables (first lhs))]
                               (recur (rest lhs)
                                      (conj built-lhs (build new-expr :Proposition (set/union subs sub)))
                                      (set/union subs sub)))))
        actfn (bound-fn [subst] 
                (when (= (count subs) (count (filter (fn [[k v]] (subst v)) subs)))
	                ;(println "Forms:" forms "\nSubs" (into {} (map (fn [[k v]] [k (:name (subst v))]) subs))) 
                  (eval-forms-with-locals (into {} (map (fn [[k v]] [k (:name (subst v))]) subs)) forms)))
        name (build rulename :Thing {})
        act (build (str "act" (.hashCode forms)) :Action {})
        ;; TODO: Note, gensymming the subrule name precludes sharing subrules
        subrules (set (map #(defrule-helper (gensym "subrule") (rest %) subs) subrules))]
    (doseq [v (vals subs)]
      (doseq [rst (seq (@restriction-set v))]
        (assert rst (ct/find-context 'BaseCT)))
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
    (doseq [p props :let [ch (build-channel p carule nil nil)]] 
      (install-channel ch p carule :i-channel))
    (doseq [a acts :let [ch (build-channel carule a nil nil)]] 
      (install-channel ch carule a :i-channel))
    (doseq [s subrules :let [ch (build-channel carule s nil nil)]] 
      (install-channel ch carule s :i-channel))))

(defn build-quantterm-channels
  "Channels are built from each restriction, to the quantified term,
   and in each indefinite, from each dep."
  [quantterm]
  (if (variable? quantterm)
    (doseq [r (@restriction-set quantterm)
            :let [ch (build-channel r quantterm nil nil)]]
      (install-channel ch r quantterm :i-channel))
    (doseq [d (@dependencies quantterm)
            :let [ch (build-channel d quantterm nil nil)]]
      (install-channel ch d quantterm :g-channel))))

(defn build-unifier-channels
  "Channels built between unifiable terms."
  [unif]
  (let [s->t (build-channel (:source unif) (:target unif) (:sourcebind unif) (:targetbind unif))]
    (cond 
      (and (@property-map (:target unif)) ((@property-map (:target unif)) :Analytic))
      (install-channel s->t (:source unif) (:target unif) :i-channel)
      
      (= (semantic-type-of (:source unif)) :WhQuestion)
      nil
      
      (or (not (@property-map (:source unif)))
          (and (@property-map (:source unif)) (not ((@property-map (:source unif)) :Analytic))))
      (install-channel s->t (:source unif) (:target unif) :i-channel))))

(defn build-internal-channels
  [rnode ants cqs]
  ;; Build the i-channels
  (doseq [a ants :let [ch (build-channel a rnode nil nil)]] 
    (install-channel ch a rnode :i-channel))
  ;; Build the u-channels
  (doseq [c cqs :let [ch (build-channel rnode c nil nil)]]
    (install-channel ch rnode c :u-channel)))

(defn build-generic-channels
  [gnode ants]
  ;; Build the g-channels
  (doseq [a ants :let [ch (build-channel a gnode nil nil)]] 
    (install-channel ch a gnode :g-channel)))

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
  [fn expr semtype substitution]
  ;(println "Building User Term... ")
  ;; fn = (first expr)
  (let [fcn (if-not (atom? fn) (build fn :Thing substitution) fn)
        cf (or
             (cf/find-frame
               (condp = (type-of fcn)
                 clojure.lang.Symbol
                 (if (wftname? (str fcn))
                   (@caseframe (get-term fcn))
                   fcn)
                 :csneps.core/Atom
                 (:name fcn)
                 :csneps.core/Molecular
                 (@caseframe fcn)
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
                    (build arg (:type rel) substitution))
          genfills (generic-fillers (set fillers))
          molnode (build-molecular-node cf fillers :csneps.core/Molecular semtype)]
                                        ;(if (seq genfills)
                                          ;(if (subtypep semtype :Generic) semtype :Generic)
                                          ;semtype))]
      (when (seq genfills)
        (dosync (alter property-map assoc molnode (set/union (@property-map molnode) #{:Generic})))
        (build-generic-channels molnode genfills))
      molnode)))

(defmulti build
  (fn [expr semtype substitution] [(type-of expr)]))

(defmethod build
;  "Returns an empty set."
  [nil] [expr semtype substitution]
  (hash-set))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name is expr
;       of the given semantic type,
;     and returns it."
  [java.lang.String] [expr semtype substitution]
  (build (symbol expr) semtype substitution))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name is expr
;       of the given semantic type,
;     and returns it."
  [java.lang.Character] [expr semtype substitution]
  (build (symbol (str expr)) semtype substitution))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the integer x
;       of the given semantic type,
;     and returns it."
  [java.lang.Integer] [expr semtype substitution]
  (build (symbol (str expr)) semtype substitution))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the long integer x
;       of the given semantic type,
;     and returns it."
  [java.lang.Long] [expr semtype substitution]
  (build (symbol (str expr)) semtype substitution))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the ratio x
;       of the given semantic type,
;     and returns it."
  [clojure.lang.Ratio] [expr semtype substitution]
  (build (symbol (str expr)) semtype substitution))

(defmethod build
;  "Creates (if necessary) an atomic term expressed by
;           the symbol whose name looks like the floating point number x,
;           rounded to *PRECISION* digits
;       of the given semantic type,
;     and returns it."
  [java.lang.Double] [expr semtype substitution]
  (build (symbol (str (roundf expr))) semtype substitution))

(defmethod build
;  "Returns the given term,
;       and if necessary, adjusting its semantic type so that
;       it is of the semantic type semtype."
  [:csneps.core/Term] [expr semtype substitution]
  (adjustType expr (semantic-type-of expr) semtype))

(defmethod build
  [clojure.lang.Symbol] [expr semtype substitution]
  ;(println "Building symbol: " expr)
  (let [term (or 
               (and (seq substitution)
                    (substitution expr))
               (get-term expr))]
    (cond
      (or (wftname? (str expr))
          (quantterm? (str expr)))
        (if term ;;Lower it's semantic type if necessary
          (adjustType term (semantic-type-of term) semtype)
          (error (str "The name " expr " is not yet associated with a term.")))
      term ;Lower its semantic type, if necessary
	    (adjustType term (semantic-type-of term) semtype)
      :else
        (let [term (new-atom {:name expr})]
          (dosync 
            (alter TERMS assoc expr term)
            (alter type-map assoc expr semtype)
            (alter support assoc term #{['hyp #{(:name term)}]}))
          (when (= expr 'True)
            (assert term (ct/find-context 'BaseCT)))
          (when (= expr 'False)
            (assert 
              (build (list 'not term) :Proposition substitution)
              (ct/find-context 'BaseCT)))
          term))))


(defmethod build
  [clojure.lang.LazySeq] [expr semtype substitution]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution))

(defmethod build
  [clojure.lang.PersistentList] [expr semtype substitution]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution))

(defmethod build
  [clojure.lang.Cons] [expr semtype substitution]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution))

(defmethod build
  [clojure.lang.PersistentVector$ChunkedSeq] [expr semtype substitution]
  ;(println (list* expr) " " (type (list* expr)))
  ;(build (list* expr) semtype substitution))
  (build (vec expr) semtype substitution))

(defmethod build
  [clojure.lang.PersistentHashSet] [expr semtype substitution] 
  (let [set (set (map #(build % semtype substitution) expr))]
    (if (= (count set) 1) (first set) set)))

(defmethod build
;    "Creates (if necessary) a well-formed term expressed by expr
;       of the given semantic type,
;     and returns it."
  [clojure.lang.PersistentVector] [expr semtype substitution]
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
          (let [entity (build (second expr) :Entity substitution)
                category (build (third expr) :Category substitution)
                genfils (if (= entity category)
                          (generic-fillers #{entity})
                          (generic-fillers #{entity category}))
                molnode (build-molecular-node (cf/find-frame 'Isa)
                                              (list entity category)
                                              :csneps.core/Categorization
                                              semtype)]
                                              ;(if (seq genfils)
                                              ;  (if (subtypep semtype :Generic) semtype :Generic)
                                              ;  semtype))]
            
            (when (seq genfils)
              (dosync (alter property-map assoc molnode (set/union (@property-map molnode) #{:Generic})))
              (build-generic-channels molnode genfils))
            molnode))
              
      and
        (if-let [cf (cf/find-frame 'and)]
          (cond
            (rest (rest expr)) ;;>1 conjunct
              (let [fillers (build (set (rest expr)) semtype substitution)]
                (build-channels (build-molecular-node cf (list fillers) :csneps.core/Conjunction semtype :fsemtype (semantic-type-of fillers))))
            (rest expr) ;;1 conjunct
               (if (or (and (seqable? (second expr)) (= (first (second expr)) 'setof))
                       (set? (second expr)))
                ;; if the argument is a set, treat it as the set of conjuncts
                (if (= (count (second expr)) 1)
                  (first (second expr))
                  (build-channels (build-molecular-node cf (rest expr) :csneps.core/Conjunction semtype :fsemtype (semantic-type-of (second expr)))))
                ;; otherwise, just build the conjunct.
                (build (second expr) semtype substitution))
            :else
              (build 'True :csneps.core/Proposition substitution))
          (error "There is no frame associated with and."))

      or
      ;; expr is (or a1 ... an)
      (if-let [cf (cf/find-frame 'andor)]
        (cond
          (rest (rest expr))
          (let [fillers (build (set (rest expr)) semtype substitution)]
            (build-channels 
              (build-molecular-node cf (list fillers) :csneps.core/Disjunction semtype :fsemtype (semantic-type-of fillers)
                                    :min 1 :max (count fillers))))
          (rest expr)
          ;; If only one disjunct, just build the disjunct.
          (build (second expr) semtype substitution)
          :else
          ;; A disjunction with no disjuncts is the False proposition
          (build 'False :csneps.core/Term substitution))
        (error "There is no frame associated with or"))

      xor
      ;; expr is (xor a1 ... an)
      (if-let [cf (cf/find-frame 'andor)]
        (cond
          (rest (rest expr))
          (let [fillers (build (set (rest expr)) semtype substitution)]
            (build-channels 
              (build-molecular-node cf (list fillers) :csneps.core/Xor semtype :fsemtype (semantic-type-of fillers)
                                    :min 1 :max 1)))
          (rest expr)
          ;; If only one disjunct, just build the disjunct.
          (build (second expr) semtype substitution)
          :else
          ;; An exclusive disjunction with no disjuncts is the False proposition
          (build 'False :csneps.core/Term substitution))
        (error "There is no frame associated with xor"))
      
      nand
       ;; expr is (nand a1 ... an)
       (if-let [cf (cf/find-frame 'andor)]
         (cond
           (or 
             (> (count expr) 2)
             (and (set? (second expr))
                  (> (count (second expr)) 1)))
           (let [fillers (build (set (rest expr)) semtype substitution)]
             (build-channels 
               (build-molecular-node cf (list fillers) :csneps.core/Nand semtype :fsemtype (semantic-type-of fillers)
                                     :min 0 :max (dec (count fillers)))))
           (= (count expr) 2)
           ;; If only one argument, build the negation of the argument.
           (build (list 'not (second expr)) semtype substitution)
           :else
           ;; A negatedconjunction with no arguments is the False proposition
           (build 'False :csneps.core/Term substitution))
         (error "There is no frame associated with nand"))

      andor
       ;; expr is (andor (i j) a1 ... an)
      (build-andor (rest expr) semtype)

      thresh
       ;; expr is  (thresh (i j) a1 ... an)
      (build-thresh (rest expr) semtype)
      
      if
        ;; expr is (if a1 a2)
      (if-let [cf (cf/find-frame 'if)]
        (do
          (checkArity 'if expr cf)
          (let [fillers1 (build (second expr) semtype substitution)
                fillers2 (build (nth expr 2) semtype substitution)]
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
          (let [fillers (build (set (rest expr)) semtype substitution)]
            (build-channels
              (build-molecular-node
                cf (list fillers) :csneps.core/Equivalence semtype
                :fsemtype (semantic-type-of fillers)
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
                  :fsemtype (semantic-type-of (second expr))
                  :min 1 :max (dec (count (first (rest expr)))))))
            (build 'True :csneps.core/Term substitution))
          :else
          ;; A iff with no fillers is the True proposition
          (build 'True :csneps.core/Term substitution))
        (error "There is no frame associated with iff."))

       (not nor)
       ;; expr is (nor a1 ... an) or (not a1 ... an)
       (if-let [cf (cf/find-frame 'nor)]
         (cond
           ;(third expr)		; at least two arguments
           (rest expr) ;;1 or more args
           (let [fillers (build (set (rest expr)) semtype substitution)]
             (build-channels
               (build-molecular-node
                 cf (list fillers) :csneps.core/Negation semtype
                 :fsemtype (semantic-type-of fillers))))
           ;(rest expr)		; exactly one argument
           ;  (build-canonical-negation (second expr) semtype)
           :else			; (not) = (nor) = T
           (build 'True :csneps.core/Term substitution))
         (error "There is no frame associated with nor."))

      (thnot thnor)
      ;; expr is (thnor a1 ... an) or (thnot a1 ... an)
      (if-let [cf (cf/find-frame 'thnor)]
        (cond
          (rest expr)		; at least one argument
          (let [fillers (build (set (rest expr)) semtype substitution)]
            (build-molecular-node
              cf (list fillers) :csneps.core/Negationbyfailure semtype
              :fsemtype (semantic-type-of fillers)))
          :else			; (thnot) = (thnor) = T
          (build 'True :csneps.core/Term substitution))
        (error "There is no frame associated with thnor."))


      setof
      (let [set (set (for [arg (rest expr)] (build arg semtype substitution)))]
        (if (= (count set) 1) (first set) set))
      
      close
      (let [vars-in-closure @(let [vars (ref #{})] 
                               (csneps.core.build/term-prewalk (fn [x] (when (csneps.core.build/variable? x) 
                                                                         (dosync (alter vars conj x))) 
                                                                 x) 
                                                               (get-term 'wft1)) 
                               vars)
            vars-name-map (into {} (map (fn [x] [(:var-label x) x]) vars-in-closure))
            closed-var-names (if (seq? (second expr)) (second expr) (list (second expr)))
            closed-vars (map vars-name-map closed-var-names)
            fillers (build (set (rest (rest expr))) :Proposition substitution)]
        
        (build-molecular-node (cf/find-frame 'close)
	                            (list fillers)
	                            :csneps.core/Closure
	                            semtype
                              :closed-vars closed-vars))
        
	      ;(build-molecular-node (cf/find-frame 'close)
	      ;                      (list (build (set closed-vars) :Entity substitution)
	      ;                            (build (third expr) :Proposition substitution))
	      ;                      :csneps.core/Molecular
	      ;                      semtype))
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
            (build-user-term fcn expr semtype substitution)))))

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
  [quant var-label rsts notsames & {:keys [arb-rsts ind-rsts qvar-rsts]}]
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
      (find-old-var-node var-label rsts arb-rsts ind-rsts qvar-rsts quant notsames))
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
       (instantiate-sem-type (:name varterm) :Entity)
        
      varterm)))


(defn pre-build-vars
  "Loops through the arbitrary individuals and builds an initial
   structure, but not the restriction sets. The same is done for the
   indefinite objects."
  [arb-rsts ind-dep-rsts qvar-rsts notsames]
  (into {}
        (concat (for [k (seq (keys arb-rsts))]
                  [k (pre-build-var :every k (get arb-rsts k) notsames :arb-rsts arb-rsts :ind-rsts ind-dep-rsts)])
                (for [k (seq (keys ind-dep-rsts))]
                  [k (pre-build-var :some k (second (get arb-rsts k)) notsames)])
                (for [k (seq (keys qvar-rsts))]
                  [k (pre-build-var :qvar k (get qvar-rsts k) notsames)]))))

(defn internal-restrict
  [var]
  (let [res (@restriction-set var)
        categorizations (filter #(isa? (syntactic-type-of %) :csneps.core/Categorization) res)
        categories (apply clojure.set/union (map #(second (@down-cableset %)) categorizations))
        semcats (filter semtype? (map #(keyword (:name %)) categories))]
    (doall (map #(adjustType var (semantic-type-of var) %) semcats))))

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
                                       [(concat (rest rsts) seen) (rest (first rsts))]
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
          (concat rsts-done finished))))))

(defn build-var
  "Build a variable node of type quant with label var-label and
   restrictions rsts. Substitution contains all the variable nodes
   that should be needed to build the restrictions. Dependencies is only
   needed for building indefinite objects."
  [quant var-label rsts substitution notsames & {:keys [deps]}]
  (let [var (substitution var-label)
        nsvars (set (map #(substitution %) (notsames var-label)))]
    (when-not (@restriction-set var) ;; already built!
      (alter TERMS assoc (:name var) var)
      (cond 
        (= quant :qvar) 
        (let [restrictions (clojure.set/union (@restriction-set var)
                                              (set (map #(build % :WhQuestion substitution) rsts)))]
          (alter restriction-set assoc var restrictions)
          (alter msgs assoc var (create-message-structure :csneps.core/QueryVariable nil)))
        (and (= quant :some) (not (nil? dependencies)))
        (let [restrictions (set (map #(build % :Propositional substitution) rsts))
              deps (clojure.set/union (@dependencies var)
                                      (set (map #(substitution %) deps)))] 
          (alter dependencies assoc var deps)
          (alter restriction-set assoc var restrictions)
          (alter msgs assoc var (create-message-structure :csneps.core/Indefinite nil))
          (dosync (doseq [r restrictions]
                    (alter property-map assoc r (set/union (@property-map r) #{:Generic :Analytic})))))
               ;(set (map #(adjustType % :Propositional :AnalyticGeneric) restrictions))))
        :else
        (let [restrictions (clojure.set/union (@restriction-set var)
                                              (set (map #(build % :Propositional substitution) rsts)))] 
          (alter restriction-set assoc var restrictions)
          (alter msgs assoc var (create-message-structure :csneps.core/Arbitrary restrictions))
          (dosync (doseq [r restrictions]
                    (alter property-map assoc r (set/union (@property-map r) #{:Generic :Analytic}))))))
            ; (set (map #(adjustType % :Propositional :AnalyticGeneric) restrictions))))
    
            
      (alter (:not-same-as var) clojure.set/union nsvars)
    
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
    (symbol? rst) ['Isa var rst]
    (and (seqable? rst)
         (not (some #(= % var) rst))) (into [(first rst) var] (rest rst))
    :else rst))

(defn expand-rsts
  "Expands rsts to account for the syntactic sugar allowed in writing them."
  [rsts]
  ;; rsts are a map from var name to restrictions. 
  (let [var-rsts-pairs (vec rsts)]
    (into {}
          (for [[var rsts] var-rsts-pairs]
            [var (vec (map #(expand-rst var %) rsts))]))))

(defn expand-ind-dep-rsts
  [rsts]
  (let [var-rsts-pairs (vec rsts)]
    (into {}
          (for [[var dep-rsts] var-rsts-pairs]
            [var (list (first dep-rsts) (vec (map #(expand-rst var %) (second dep-rsts))))]))))

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
    (and (seqable? assertion-spec) (not (set? assertion-spec)) (not (map? assertion-spec)))
    (cond
      (= (first assertion-spec) 'some)
      (let [rsts (rest (ind-deps-rsts (second assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference rsts (rest (rest (rest assertion-spec))))))
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
      (= (first assertion-spec) 'every)
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
                ar (assoc ar
                          (second assertion-spec)
                          (if (= (rest (rest assertion-spec)) '())
                            (list (list 'Isa (second assertion-spec) 'Entity)) ;; (every x) is shortcut for (every x (Isa x Entity)) DRS [3/31/12]
                            aspec))]
            [(second assertion-spec) ar idr qvr])))
      (synvariable? (first assertion-spec))
      (let [rsts (second (qvar-rsts (first assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference rsts (rest assertion-spec))))
          (error (str "Restriction sets: " rsts " and " (rest assertion-spec) " cannot be applied to
                      the same variable."))
          :else
          (let [[aspec ar idr qvr] (parse-vars-and-rsts (rest assertion-spec) 
                                                        arb-rsts ind-deps-rsts qvar-rsts
                                                        :buildingqvar (conj buildingqvar (first assertion-spec)))
                qvr (assoc qvar-rsts
                           (first assertion-spec)
                           (if (= (rest assertion-spec) '())
                             (list (list 'Isa (first assertion-spec) 'Entity))
                             aspec))]
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

(defn check-and-build-variables
  "Given a top-level build expression, checks that expression for
   variable terms syntax (e.g., every, some). These terms are built and
   a new build expression is created with just the variable-labels
   (e.g., x in (every x ...)). This and a substitution between
   variable-labels and the AI and IO is provided to build.
   Returns three values. The first is the new build expression, the
   second the the built nodes, and the third the substitution between
   var-labels and the AI/IO nodes."
  [expr]
  (dosync
    (let [[new-expr arb-rsts ind-dep-rsts qvar-rsts] (parse-vars-and-rsts expr {} {} {})
          arb-rsts (generate-notsames (expand-rsts arb-rsts))
          qvar-rsts (generate-notsames (expand-rsts qvar-rsts))
          ind-dep-rsts (expand-ind-dep-rsts ind-dep-rsts)
          [arb-rsts qvar-rsts ind-dep-rsts notsames] (notsames arb-rsts qvar-rsts ind-dep-rsts)
          substitution (pre-build-vars arb-rsts ind-dep-rsts qvar-rsts notsames)
          built-vars (build-vars arb-rsts ind-dep-rsts qvar-rsts substitution notsames)]
      [new-expr built-vars substitution])))
