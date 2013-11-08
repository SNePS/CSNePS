(in-ns 'csneps.core)

;;; Data structures for Syntactic Types
;;; ====================================

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

(defn ind-counter [] (+ 1 @INDCOUNT))

(defn arb-counter [] (+ 1 @ARBCOUNT))

(defn qvar-counter [] (+ 1 @QVARCOUNT))

(defn wft-counter [] @WFTCOUNT)

(defn inc-wft-counter []
  (dosync (alter WFTCOUNT inc)))

(defn inc-arb-counter []
  (dosync (alter ARBCOUNT inc)))

(defn inc-ind-counter []
  (dosync (alter INDCOUNT inc)))

(defn inc-qvar-counter []
  (dosync (alter QVARCOUNT inc)))

;;;TODO: Initialize instance... Maybe we'll modify our custom record?

(defn type-of
  [expr]
  (let [stype (ref true)]
      (cond
        (:type expr) (:type expr)
        (keyword? expr) expr
        true (type expr))))

(defn term? 
  [o]
  (when (:type o) (isa? (:type o) ::Term)))

(defn molecularTerm? 
  [o]
  (when (:type o) (isa? (:type o) ::Molecular)))

(defn variableTerm? 
  [o]
  (when (:type o) (isa? (:type o) ::Variable)))

(defn arbitraryTerm? 
  [o]
  (when (:type o) (isa? (:type o) ::Arbitrary)))

(defn indefiniteTerm?
  [o]
  (when (:type o) (isa? (:type o) ::Indefinite)))

(defn atomicTerm? 
  [o]
  (when (:type o) (isa? (:type o) ::Atom)))

;;; Syntactic Types
;;; ===============

(defvar TopSyntacticType ::Term
  "The root of the syntactic type hierarchy.")

;(defprotocol SYNTACTIC-TYPE
;  (syntactic-type-of [t])
;  (stype [t]))

(defn syntactic-type-of
  [term]
  (:type term))

;;;Instantiate with (new-test {}) and inside {} use any args you need.
(defrecord2 Term
  [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   type ::Term])

;;;Portnote: was named "atom", Clojure doesn't like this.
(defrecord2 Atom
  [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   type ::Atom])

(defrecord2 Base
  [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   type ::Base])

(defrecord2 Variable
  [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Variable
   restriction-set (ref (hash-set))
   var-label nil
   type ::Variable])

(defrecord2 Indefinite
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Variable
   restriction-set (ref (hash-set))
   var-label nil
   ;;Additions for Indefinite
   dependencies (ref (hash-set))
   type ::Indefinite])

(defrecord2 Arbitrary
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;; Arbitrary specific:
   lattice-node (ref nil)
   ;;Additions for Variable
   restriction-set (ref (hash-set))
   var-label nil
   type ::Arbitrary])

(defrecord2 QueryVariable
  [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Variable
   restriction-set (ref (hash-set))
   var-label nil
   type ::QueryVariable])

(defrecord2 Molecular
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Molecular])

(defrecord2 Rule
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Rule
   rhsfn nil
   type ::Rule])


(defrecord2 Param2op
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Param2op])

(defrecord2 Andor
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Andor])

(defrecord2 Disjunction
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Disjunction])

(defrecord2 Xor
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Xor])

(defrecord2 Nand
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Nand])

(defrecord2 Thresh
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Thresh])

(defrecord2 Equivalence
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   ;;Additions for Param2op
   min nil
   max nil
   type ::Equivalence])

(defrecord2 Conjunction
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Conjunction])

(defrecord2 Negation
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Negation])

(defrecord2 Negationbyfailure
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Negationbyfailure])

(defrecord2 Numericalentailment
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Numericalentailment])

(defrecord2 Orentailment
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Orentailment])

(defrecord2 Implication
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Implication])

(defrecord2 Categorization
   [name nil
   activation-value 0.0
   fired nil
   recorded-firing nil
   activation-marker nil
   i-channels (ref (hash-set))
   y-channels (ref (hash-set))
   ant-in-channels (ref (hash-set))
   instances (ref (hash-map))
   msgs (ref nil)
   up-cablesetw (ref (hash-map))
   support (ref (hash-set))
   ;;Additions for Molecular
   caseframe nil
   down-cableset '()
   down-weights (ref '())
   type ::Categorization])


(defn define-syn-type
  ""
  [newtype super]
  (derive newtype super)) 

;;Hierarchy:
;(dosync
  (define-syn-type ::Atom ::Term)
  (define-syn-type ::Base ::Atom)
  (define-syn-type ::Variable ::Atom)
  (define-syn-type ::Indefinite ::Variable)
  (define-syn-type ::Arbitrary ::Variable)
  (define-syn-type ::QueryVariable ::Variable)
  (define-syn-type ::Molecular ::Term)
  (define-syn-type ::Rule ::Molecular)
  (define-syn-type ::Param2op ::Molecular)
  (define-syn-type ::Andor ::Param2op)
  (define-syn-type ::Disjunction ::Andor)
  (define-syn-type ::Xor ::Andor)
  (define-syn-type ::Nand ::Andor)
  (define-syn-type ::Thresh ::Param2op)
  (define-syn-type ::Equivalence ::Thresh)
  (define-syn-type ::Conjunction ::Molecular)
  (define-syn-type ::Negation ::Molecular)
  (define-syn-type ::Negationbyfailure ::Molecular)
  (define-syn-type ::Numericalentailment ::Molecular)
  (define-syn-type ::Orentailment ::Numericalentailment)
  (define-syn-type ::Implication ::Numericalentailment)
  (define-syn-type ::Categorization ::Molecular);)

;;; syntype -> new-fn map
;;; =====================

;; One of the slowest operations we perform, and we do it a lot, is 
;; building the symbols to get the function which builds new syntypes.
;; So, we cache those functions here, easily searchable by their keywords.

(def SYNTYPE-NEWFN-MAP
  {::Term (eval 'csneps.core/new-term)
   ::Atom (eval 'csneps.core/new-atom)
   ::Base (eval 'csneps.core/new-base)
   ::Variable (eval 'csneps.core/new-variable)
   ::Indefinite (eval 'csneps.core/new-indefinite)
   ::Arbitrary (eval 'csneps.core/new-arbitrary)
   ::QueryVariable (eval 'csneps.core/new-query-variable)
   ::Molecular (eval 'csneps.core/new-molecular)
   ::Rule (eval 'csneps.core/new-rule)
   ::Param2op (eval 'csneps.core/new-param2op)
   ::Andor (eval 'csneps.core/new-andor)
   ::Disjunction (eval 'csneps.core/new-disjunction)
   ::Xor (eval 'csneps.core/new-xor)
   ::Nand (eval 'csneps.core/new-nand)
   ::Thresh (eval 'csneps.core/new-thresh)
   ::Equivalence (eval 'csneps.core/new-equivalence)
   ::Conjunction (eval 'csneps.core/new-conjunction)
   ::Negation (eval 'csneps.core/new-negation)
   ::Negationbyfailure (eval 'csneps.core/new-negationbyfailure)
   ::Numericalentailment (eval 'csneps.core/new-numericalentailment)
   ::Orentailment (eval 'csneps.core/new-orentailment)
   ::Implication (eval 'csneps.core/new-implication)
   ::Categorization (eval 'csneps.core/new-categorization)})
  
;;; Functions on Syntactic Types
;;; =============================

(defn install-in-upcset
  "Installs n in the up-cableset of m for relation r."
  [n r m]
    (when (not (contains? @(:up-cablesetw m) r))
      (dosync (alter (:up-cablesetw m) assoc r (ref (hash-set)))))
    (dosync (alter (second (find @(:up-cablesetw m) r)) conj n)))

(defn totparam [term]
  "Returns the tot of the given param2op term."
  (count (first (:down-cableset term))))

(defn change-type
  [term newtype]
  (assoc term :syntype newtype, :type (keyword (str 'sneps3\/ (st/capitalize (str newtype))))))

(defn get-term
  [term]
  (if (isa? (type-of term) ::Term)
    term
    (get @TERMS term)))

(defn is-syntactic-type?
  [x]
  (isa? x ::Term))

(defn get-new-syntype-fn
  [synname]
  (synname SYNTYPE-NEWFN-MAP))

(defn build-upcsets
  [mol]
  (doseq [[rel ns] (map #(vector %1 %2) (:slots (:caseframe mol)) (:down-cableset mol))]
    (if (= (type ns) clojure.lang.PersistentHashSet)
      (doseq [m ns]
        (install-in-upcset mol rel m))
      (install-in-upcset mol rel ns))))

(defmulti initialize-syntype
  (fn [term & rest] [(type-of term)]))

(defmethod initialize-syntype
  [::Molecular] [term & rest]
  (build-upcsets term))