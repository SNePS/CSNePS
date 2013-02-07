(ns csneps.core.build
  (:require [clojure.math.combinatorics :as cb])
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.printer :as print]
            [csneps.core.relations :as slot]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as cb]
            [clojure.zip :as zip]
            [clojure.set :as set])
  ;(:refer-clojure :exclude [assert]) ;;Possible bug: This breaks loading clojure.math.combinatorics for no reason?
  (:use [csneps.core]
        [csneps.util]
        [clojure.walk :as walk :only [prewalk]]
        [csneps.core.find-utils]))

;(refer-clojure :exclude '[assert])

(declare assert build check-and-build-variables build-channels)

(load "build_assert")
(load "build_utils")
(load "build_substitution")
(load "build_unification")
(load "build_find")
(load "build_channel")

(defvar KRNovice nil
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
           (let [i (subs fname 1 (- len 1))]
             (or (= i "v")
                 (every? digit-char-p i)))))))

(defn ientaili
  "Assuming that i=> satisfies #'ientailsymb?,
       returns the number represented by i."
  [=i>]
  (let* [fname (str =i>)
	 len (.length fname)
	 i (.substring fname 1 (- len 1))]
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
  (let [correctlength
	 (+ (count (:slots cf))
	    (if (cf/quotedpp? cf) 1 0))]
    (when-not (= (count expr) correctlength)
      (error fn " is used with incorrect arity in " expr ".\n
                         It should be given " (- correctlength 1) " arguments instead of " (- (count expr) 1) "."))))

(defn adjustType
  "Adjusts the type of term, if possible,
       from its old semantic type, oldtype,
       to its new semantic type, newtype,
       while keeping its syntactic type, syntype,
    and return the term."
  [term oldtype newtype]
  ;(println "Adjusting type of: " (:name term) " from: " oldtype " -> " newtype)
  (let [gcsub (ref nil)]
    (cond
      ;;Types are already the same
      (= (type-of term) (type-of newtype)) nil
      ;;Newtype is a subtype of oldtype
      (subtypep newtype oldtype)
        (dosync (alter type-map assoc (:name term) newtype))
      ;; If new type and oldtype have a common subtype,
      ;;    change the type to be the greatest common subtype
      ;; Could there be more than 1 greatest common subtype?
      (dosync (ref-set gcsub (gcsubtype newtype oldtype)))
        (if (> (count @gcsub) 1)
          (let [gcsubchoice (menuChooseFromList (str "Choose a type for " term ".") @gcsub)]
            (if gcsubchoice
              (dosync (alter type-map assoc (:name term) gcsubchoice))
              (error "No semantic type for " term ".")))
          (dosync (alter type-map assoc (:name term) (first @gcsub))))
      :else (error "Type error")))
;  (println "Done Adjusting")
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
  [cf dcs syntype semtype & {:keys [fsemtype min max]}]
  ;(println "building molecular..." dcs (doall (map make-set-if-not-set dcs)))
  (let [dcs-sets (map make-set-if-not-set dcs)
        tests (doall (map #(check-min-max %1 %2) dcs-sets (:slots cf)))
        term (or
               (cond 
                 max (find-exact syntype cf dcs-sets :min min :max max)
                 min (find-exact syntype cf dcs-sets :min min)
                 :else (find-exact syntype cf dcs-sets))
               (let [wft ((get-new-syntype-fn syntype) {:name (symbol (str "wft" (inc-wft-counter)))
                                                        :caseframe cf
                                                        :down-cableset dcs-sets
                                                        :min (if (nil? min) 0 min)
                                                        :max (if (nil? max) 0 max)})]
                 (initialize-syntype wft)
                 (dosync 
                   (alter TERMS assoc (:name wft) wft)
                   (alter type-map assoc (:name wft) (:type cf)))

                 ;;Now that we made it, add it to the unif tree.
                 (addTermToUnificationTree wft)

                 (cf/add-caseframe-term wft :cf cf)
                 wft))]
    ;(println "term: " term)
    
    (adjustType term (:type cf) (if fsemtype fsemtype semtype))))


(defn build-andor
  "Build a term for andor
       args is the original expression after 'andor'."
  [args semtype]
  (let [cf (cf/find-frame 'andor)]
    (when-not cf (error "There is no frame associated with andor."))
    (when-not (seq? (first args))
      (error "andor must be followed by a list, (i j) in " (list* 'andor args) "."))
    (let [min (first (first args))
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
        (and (= min 0) (= max tot))
          (build 'T semtype {})
        (= tot min max)
          (build (list* 'and (rest args)) semtype {})
        (= 0 min max)
          (build (list* 'nor (rest args)) semtype {})
        (and (= min 0) (= max (- tot 1)))
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
    (when-not (seq? (first args))
      (error
       "thresh must be followed by a list, (i) or (i j), in "
         (list* 'thresh args) "."))
    (let [min (first (first args))
          tot (count (rest args))
          tot-1 (- tot 1)
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
        (and (= min 0) (= max tot))
          (build 'F semtype {})
        (and (= min 0) (= max (- tot 1)))
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
  [i ant cq semtype]
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
        (= i 0)
          (build (list 'and (build cq semtype)) semtype {})
        :else 
          (let [term (build-channels
                       (build-molecular-node
                         cf (list (build ant semtype {}) (build cq semtype {}))
                         :csneps.core/Numericalentailment semtype
                         :fsemtype semtype :min i))]
            term)))))

(defn build-internal-channels
  [rnode ants cqs]
  ;; Build the i-channels
  (doseq [a ants :let [ch (build-channel a rnode nil nil)]] 
    (dosync 
      (alter (:i-channels a) conj ch)
      (alter (:ant-in-channels rnode) conj ch)))
  ;; Build the y-channels
  (doseq [c cqs :let [ch (build-channel rnode c nil nil)]]
    (dosync 
      (alter (:y-channels rnode) conj ch)
      (alter (:ant-in-channels c) conj ch)))
  ;; Build the RUI structure
  (create-rui-structure rnode))

(defn build-channels
  [rnode]
  (let [slot-map (cf/dcsRelationTermsetMap rnode)]
    (case (type-of rnode)
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

;  "Creates (if necessary) a canonical term
;        which is the negation of the given thresh.
;        of the given semantic type,
;        and returns it."
;(defmulti build-canonical-negation
;  (fn [arg semtype] [(type-of arg)]))
;
;(defmethod build-canonical-negation
;  [true] [arg semtype]
;  (let [filler (build arg semtype {})]
;    (build-molecular-node (cf/find-frame 'nor) (list filler)
;			::Negation semtype :fsemtype (semantic-type-of filler))))
;
;(defmethod build-canonical-negation
;  [clojure.lang.LazySeq] [arg semtype]
;  (build-canonical-negation (vec arg) semtype))
;
;(defmethod build-canonical-negation
;  [clojure.lang.PersistentList] [arg semtype]
;  (build-canonical-negation (vec arg) semtype))
;
;(defmethod build-canonical-negation
;  [clojure.lang.Cons] [arg semtype]
;  (build-canonical-negation (vec arg) semtype))
;
;(defmethod build-canonical-negation
;  [clojure.lang.PersistentVector$ChunkedSeq] [arg semtype]
;  (build-canonical-negation (vec arg) semtype))
;
;(defmethod build-canonical-negation
;  [clojure.lang.PersistentVector] [arg semtype]
;  (case (first arg)
;    and
;     (build (list* 'nand (rest arg)) semtype {})
;    or
;      (build (list* 'nor (rest arg)) semtype {})
;    nand
;      (build (list* 'and (rest arg)) semtype {})
;    (or thnot thnor)
;      (build (list* 'or (rest arg)) semtype {})
;    (or nor not)
;      (build (list* 'or (rest arg)) semtype {})
;    xor
;      (build (list* 'thresh '(1 1) (rest arg)) semtype {})
;    andor
;      (build (list* 'thresh (rest arg)) semtype {})
;    thresh
;      (build (list* 'andor (rest arg)) semtype {})
;    iff
;      (build (list* 'andor `(1 ~(- (count (rest arg)) 1)) (rest arg))
;		semtype {})
;    :else
;      (let [filler (build arg semtype {})]
;        (build-molecular-node (cf/find-frame 'nor) (list filler)
;			   ::Negation semtype :fsemtype (semantic-type-of filler)))))
;
;
;(defmethod build-canonical-negation
;  [::Disjunction] [arg semtype]
;  (build `(nor ~@(seq (find/findto arg 'andorargs)))
;	 semtype {}))
;
;(defmethod build-canonical-negation
;  [::Conjunction] [arg semtype]
;  (build `(nand ~@(seq (find/findto arg 'and)))
;	 semtype {}))
;
;(defmethod build-canonical-negation
;  [::Nand] [arg semtype]
;  (build `(and ~@(seq (find/findto arg 'andorargs)))
;	 semtype {}))
;
;(defmethod build-canonical-negation
;  [::Negation] [arg semtype]
;  (build `(or ~@(seq (find/findto arg 'nor)))
;	 semtype {}))
;
;(defmethod build-canonical-negation
;  [::Negationbyfailure] [arg semtype]
;  (build `(or ~@(seq (find/findto arg 'thnor)) )
;	 semtype {}))
;
;(defmethod build-canonical-negation
;  [::Andor] [arg semtype]
;  (build `(thresh (~(:min arg) ~(:max arg))
;		  ~@(seq (find/findto arg 'andorargs)))
;	 semtype {}))
;
;(defmethod build-canonical-negation
;  [::Thresh] [arg semtype]
;  (build `(andor (~(:min arg) ~(:max arg))
;		 ~@(seq (find/findto arg 'threshargs)))
;	 semtype {}))

(defn build-user-term
  "Build a term for the expression expr, whose function is fcn,
       and whose contextual semantic type is to be semtype."
  [fn expr semtype substitution]
  ;(println "Building User Term... ")
  ;; fn = (first expr)
  (let [fcn (if (not (atom? fn)) (build fn :Thing substitution) fn)
        cf (or
             (cf/find-frame
               (condp = (type-of fcn)
                 clojure.lang.Symbol
                 (if (wftname? (str fcn))
                   (:caseframe (get-term fcn))
                   fcn)
                 :csneps.core/Atom
                 (:name fcn)
                 :csneps.core/Molecular
                 (:caseframe fcn)
                 (error
                   "The function \"symbol\", "fcn", is not an acceptable function \"symbol\".")))
             (and KRNovice
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
          fillers (for [[arg rel] (map #(vector %1 %2) 
                                       (if (cf/quotedpp? cf)
                                         (rest fixed-expr)
                                         fixed-expr)
                                       (:slots cf))]
                    (build arg (:type rel) substitution))]
      (build-molecular-node cf fillers :csneps.core/Molecular semtype))))

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
               (and (not (empty? substitution))
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
        (let [newatom (new-atom {:name expr})]
          (dosync 
            (alter TERMS assoc expr newatom)
            (alter type-map assoc expr semtype))
          (when (= expr 'T)
            (assert term (ct/find-context 'BaseCT)) :hyp)
          (when (= expr 'F)
            (assert (build (list 'not term) :Proposition substitution)
              (ct/find-context 'BaseCT)) :hyp)
          newatom))))


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
  ;[clojure.lang.PersistentList] [expr semtype substitution]
  ;[clojure.lang.Cons] [expr semtype substitution]
  [clojure.lang.PersistentVector] [expr semtype substitution]
  (let [fcn (first expr)]
    ;(println "Building: " expr)
    ;(println "Subs:" substitution)
    (case fcn
      ;; Only functions with special syntax
      ;;    or which build terms of a special syntactic type
      ;;    need to be handeled specially.
      ;; All others can use the default case of buildUserTerm.
      Isa 
        (do
          ;(println "Isa...")
          (if (not (= (count expr) 3))
            (error (str "Isa must take 2 arguments. It doesn't in " expr ".")))
            (build-molecular-node (cf/find-frame 'Isa)
                                  (list (build (second expr) :Entity substitution)
                                        (build (third expr) :Category substitution))
                                  :csneps.core/Categorization
                                  semtype))

      and
        (let [cf (cf/find-frame 'and)]
          (when-not cf (error "There is no frame associated with and."))
          (cond
            (rest (rest expr)) ;;>1 conjunct
              (let [fillers (build (set (rest expr)) semtype substitution)]
                (build-channels (build-molecular-node cf (list fillers) :csneps.core/Conjunction semtype :fsemtype (semantic-type-of fillers))))
            (rest expr) ;;1 conjunct
               (if (or (and (seq? (second expr)) (= (first (second expr)) 'setof))
                       (set? (second expr)))
                ;; if the argument is a set, treat it as the set of conjuncts
                (if (= (count (second expr)) 1)
                  (first (second expr))
                  (build-channels (build-molecular-node cf (rest expr) :csneps.core/Conjunction semtype :fsemtype (semantic-type-of (second expr)))))
                ;; otherwise, just build the conjunct.
                (build (second expr) semtype substitution))
            :else
              (build 'T :csneps.core/Proposition substitution)))

      or
      ;; expr is (or a1 ... an)
      (let [cf (cf/find-frame 'andor)]
        (when-not cf (error "There is no frame associated with or"))
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
          (build 'F :csneps.core/Term substitution)))

      xor
      ;; expr is (xor a1 ... an)
      (let [cf (cf/find-frame 'andor)]
        (when-not cf (error "There is no frame associated with xor"))
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
          (build 'F :csneps.core/Term substitution)))
      
      nand
       ;; expr is (nand a1 ... an)
       (let [cf (cf/find-frame 'andor)]
         (when-not cf (error "There is no frame associated with nand"))
         (cond
           (or 
             (> (count expr) 2)
             (and (set? (second expr))
                  (> (count (second expr)) 1)))
           (let [fillers (build (set (rest expr)) semtype substitution)]
             (build-channels 
               (build-molecular-node cf (list fillers) :csneps.core/Nand semtype :fsemtype (semantic-type-of fillers)
                                     :min 0 :max (- (count fillers) 1))))
           (= (count expr) 2)
           ;; If only one argument, build the negation of the argument.
           (build (list 'not (second expr)) semtype substitution)
           :else
           ;; A negatedconjunction with no arguments is the False proposition
           (build 'F :csneps.core/Term substitution)))

      andor
       ;; expr is (andor (i j) a1 ... an)
      (build-andor (rest expr) semtype)

      thresh
       ;; expr is  (thresh (i j) a1 ... an)
      (build-thresh (rest expr) semtype)
      
      if
        ;; expr is (if a1 a2)
      (let [cf (cf/find-frame 'if)]
        (when-not cf (error "There is no frame associated with if"))
        (checkArity 'if expr cf)
        (let [fillers1 (build (second expr) semtype substitution)
              fillers2 (build (nth expr 2) semtype substitution)]
          (build-channels 
            (build-molecular-node
              cf (list fillers1 fillers2) :csneps.core/Implication semtype
              :fsemtype semtype :min (if (set? fillers1)
                                       (count fillers1)
                                       1)))))

      iff
       ;; expr is (iff a1 ... an)
      (let [cf (cf/find-frame 'thresh)]
        (when-not cf (error "There is no frame associated with iff."))
        (cond
          (rest (rest expr))
          (let [fillers (build (set (rest expr)) semtype substitution)]
            (build-channels
              (build-molecular-node
                cf (list fillers) :csneps.core/Equivalence semtype
                :fsemtype (semantic-type-of fillers)
                :min 1 :max (- (count fillers) 1))))
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
                  :min 1 :max (- (count (first (rest expr))) 1))))
            (build 'T :csneps.core/Term substitution))
          :else
          ;; A iff with no fillers is the True proposition
          (build 'T :csneps.core/Term substitution)))

       (not nor)
       ;; expr is (nor a1 ... an) or (not a1 ... an)
       (let [cf (cf/find-frame 'nor)]
         (when-not cf (error "There is no frame associated with nor."))
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
           (build 'T :csneps.core/Term substitution)))

      (thnot thnor)
      ;; expr is (thnor a1 ... an) or (thnot a1 ... an)
      (let [cf (cf/find-frame 'thnor)]
        (when-not cf (error "There is no frame associated with thnor."))
        (cond
          (rest expr)		; at least one argument
          (let [fillers (build (set (rest expr)) semtype substitution)]
            (build-molecular-node
              cf (list fillers) :csneps.core/Negationbyfailure semtype
              :fsemtype (semantic-type-of fillers)))
          :else			; (thnot) = (thnor) = T
          (build 'T :csneps.core/Term substitution)))


      setof
      (let [set (set (for [arg (rest expr)] (build arg semtype substitution)))]
        (if (= (count set) 1) (first set) set))

      (cond ;;Else caseframes
        (ientailsymb? fcn)
            ;; expr is (i=> ant cq)
            (build-numerical-entailmant (ientaili fcn) (second expr) (third expr) semtype)
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
  [quant var-label rsts & {:keys [arb-rsts ind-rsts]}]
  ;(println "Building..." var-label rsts)
  (doseq [rst rsts
            :when (not (some #{var-label} rst))]
      (error
	  (str "The variable label, " var-label ", is not part of the restriction proposition, " rst ".")))

  ;(println "rsts:" rsts "arb-rsts" arb-rsts "ind-rsts" ind-rsts)
  (or (and (= quant 'every) (find-old-arb-node var-label rsts arb-rsts ind-rsts))
    (let [name (case quant
                 every (symbol (str "arb" (arb-counter)))
                 some (symbol (str "ind" (ind-counter)))
                 qvar (symbol (str "qvar" (qvar-counter))))
                 
                 ;(= quant 'every) (symbol (str "arb" (arb-counter))) (symbol (str "ind" (ind-counter))))
          varterm (case quant
                    every (new-arbitrary {:name name :var-label var-label})
                    some (new-indefinite {:name name :var-label var-label})
                    qvar (new-query-variable {:name name :var-label var-label}))]
      (case quant 
        every (inc-arb-counter)
        some (inc-ind-counter)
        qvar (inc-qvar-counter))
      (instantiate-sem-type (:name varterm) :Entity)
      ;(println "Varterm: " varterm)
      varterm)))


(defn pre-build-vars
  "Loops through the arbitrary individuals and builds an initial
   structure, but not the restriction sets. The same is done for the
   indefinite objects."
  [arb-rsts ind-dep-rsts qvar-rsts]
  ;(println "Arb rsts" arb-rsts)
  (into {}
        (concat (for [k (seq (keys arb-rsts))]
                  [k (pre-build-var 'every k (get arb-rsts k) :arb-rsts arb-rsts :ind-rsts ind-dep-rsts)])
                (for [k (seq (keys ind-dep-rsts))]
                  [k (pre-build-var 'some k (second (get arb-rsts k)))])
                (for [k (seq (keys qvar-rsts))]
                  [k (pre-build-var 'qvar k (second (get arb-rsts k)))]))))
  
  
;  (doseq [k (seq (keys arb-rsts))]
;    (subs/add-var-term-pair
;	    k
;	    (pre-build-var 'every k (get arb-rsts k) :arb-rsts arb-rsts :ind-rsts ind-dep-rsts)
;	    substitution))
;  (doseq [k (seq (keys ind-dep-rsts))]
;    (subs/add-var-term-pair
;	    k
;	    (pre-build-var 'some k (second (get arb-rsts k)))
;	    substitution)))

(defn build-var
  "Build a variable node of type quant with label var-label and
   restrictions rsts. Substitution contains all the variable nodes
   that should be needed to build the restrictions. Dependencies is only
   needed for building indefinite objects."
  [quant var-label rsts substitution & {:keys [dependencies]}]
  ;(println "Building var " var-label)
;  (println rsts)
  
;  (println (map #(print-str %) rsts))
  
  (let [var (substitution var-label)
        rst-set-printable (clojure.string/join " " (map #(seq %) rsts))]

    (dosync 
      (alter (:restriction-set var) clojure.set/union 
             (set (map #(build % :Proposition substitution) rsts)))
      (ref-set (:restriction-set-printable var) rst-set-printable))

    (when (and (= quant 'some) (not (nil? dependencies)))
      (let [deps-printable (clojure.string/join "" (map str dependencies))]
        (dosync
          (alter (:dependencies var) clojure.set/union
                 (doall (map #(substitution %) dependencies)))
          (ref-set (:dependencies-printable var) deps-printable))))
    
    (dosync 
      (alter TERMS assoc (:name var) var)
      (case quant
        every (alter ARBITRARIES conj var)
        some  (alter INDEFINITES conj var)
        qvar  (alter QVARS conj var)))
    
    var))

(defn build-vars
  "Loops through the arbs and inds. and builds their restriction
   sets. In the case of indenfinite-objects, their dependency lists
   are populated."
  [arb-rsts ind-dep-rsts qvar-rsts substitution]
  (set 
    (concat
      (for [k (keys arb-rsts)]
        (build-var 'every k (get arb-rsts k) substitution))
      (for [k (keys ind-dep-rsts)]
        (build-var 'some k (second (get ind-dep-rsts k)) substitution 
                   :dependencies (first (get ind-dep-rsts k))))
      (for [k (keys qvar-rsts)]
        (build-var 'qvar k (get qvar-rsts k) substitution)))))

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
  [assertion-spec arb-rsts ind-deps-rsts qvar-rsts]
  ;(println "Aspec: " assertion-spec " Arbs: " @arb-rsts)
  (if (seq? assertion-spec)
    (cond
      (= (first assertion-spec) 'some)
      (let [rsts (rest (get @ind-deps-rsts (second assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference rsts (rest (rest (rest assertion-spec))))))
          (error (str "Restriction sets: " rsts " and " (rest (rest assertion-spec)) " cannot be applied to
                      the same variable."))
          :else 
          (do
            (dosync (alter ind-deps-rsts assoc (second assertion-spec)
                           (list (nth assertion-spec 2)
                                 (parse-vars-and-rsts (rest (rest (rest assertion-spec)))
                                                      arb-rsts ind-deps-rsts qvar-rsts))))
            (second assertion-spec))))
      (or (= (first assertion-spec) 'every)) ;;qvar: (synvariable? (first assertion-spec)))
      (let [rsts (second (get @arb-rsts (second assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference rsts (rest (rest assertion-spec)))))
          (error (str "Restriction sets: " rsts " and " (rest (rest assertion-spec)) " cannot be applied to
                      the same variable."))
          :else
          (do
            ;(println rsts assertion-spec)
            (dosync (alter arb-rsts assoc (second assertion-spec)
                           (if (= (rest (rest assertion-spec)) '())
                             (parse-vars-and-rsts (list (list 'Isa (second assertion-spec) 'Entity)) arb-rsts ind-deps-rsts qvar-rsts) ;; (every x) is shortcut for (every x (Isa x Entity)) DRS [3/31/12]
                             (parse-vars-and-rsts (rest (rest assertion-spec))
                                                  arb-rsts ind-deps-rsts qvar-rsts))))
            (second assertion-spec))))
      (synvariable? (first assertion-spec))
      (let [rsts (second (get @qvar-rsts (first assertion-spec)))]
        (cond
          (and rsts (not (clojure.set/difference rsts (rest assertion-spec))))
          (error (str "Restriction sets: " rsts " and " (rest assertion-spec) " cannot be applied to
                      the same variable."))
          :else
          (do
            (println rsts assertion-spec)
            (dosync (alter qvar-rsts assoc 
                           (first assertion-spec)
                           (parse-vars-and-rsts (rest assertion-spec) arb-rsts ind-deps-rsts qvar-rsts)))
            (first assertion-spec))))
      :else
      (doall (map #(parse-vars-and-rsts % arb-rsts ind-deps-rsts qvar-rsts) assertion-spec)))
    assertion-spec))

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
    (let [arb-rsts (ref (hash-map))
          ind-dep-rsts (ref (hash-map))
          qvar-rsts (ref (hash-map))
          new-expr (parse-vars-and-rsts expr arb-rsts ind-dep-rsts qvar-rsts)
          substitution (pre-build-vars @arb-rsts @ind-dep-rsts @qvar-rsts)
          built-vars (build-vars @arb-rsts @ind-dep-rsts @qvar-rsts substitution)]
      ;(println qvar-rsts)
      ;(println substitution)
      ;(println "NX: " new-expr)
      ;(println arb-rsts)
      ;(pre-build-vars @arb-rsts @ind-dep-rsts substitution)
      ;(dosync (ref-set built-vars (build-vars @arb-rsts @ind-dep-rsts substitution)))
      ;(println "Built" built-vars)
      [new-expr built-vars substitution]))

;(load "assert")
