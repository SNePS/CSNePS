(in-ns 'csneps.core.build)

(defn ignore-variable? [sym] (= '_ sym))

(def variable?
  (fn [term]
    (variableTerm? term)))

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
    ((type-of term) syntype-fsym-map)
    (let [p (:print-pattern (@term-caseframe-map term))]
      (if (and (seq? (first p)) (= (first (first p)) 'quote))
        (second (first p))
        (:name (first (first (@down-cableset term))))))))

(defn term-walk
  [inner outer termpart & {:keys [ignore-type with-restrictions]}]
  (cond
    (molecularTerm? termpart) (outer (build 
                                       (if-let [fsym (or ((type-of termpart) syntype-fsym-map)
                                                         (let [p (:print-pattern (@term-caseframe-map termpart))]
                                                           (when (and (seq? (first p)) (= (first (first p)) 'quote))
                                                             (second (first p)))))]
                                         (conj (doall (map inner (@down-cableset termpart))) fsym)
                                         (doall (map inner (@down-cableset termpart))))
                                       (if ignore-type :Entity (csneps.core/semantic-type-of termpart))
                                       {}
                                       #{}))
    (atomicTerm? termpart) (outer termpart)
    (set? termpart) (set (doall (map inner termpart)))
    :else (error (str "Term contains unknown parts (" termpart ")"))))

(defn term-recur
  [inner outer termpart]
  (cond
    (molecularTerm? termpart) (outer (build (conj (doall (map inner (@down-cableset termpart))) (term-predicate termpart)) 
                                        :Propositional
                                        {}
                                        #{}))
    ;(arbitraryTerm? termpart) (outer (build-variable (list 'every (:var-label termpart) (map inner @(:restriction-set termpart))))) 
    (atomicTerm? termpart) (outer termpart)
    (set? termpart) (set (doall (map inner termpart)))
    :else (error (str "Term contains unknown parts (" termpart ")"))))

(defn term-prewalk
  [f term & {:keys [ignore-type with-restrictions]}]
  (term-walk 
    (fn [t] (term-prewalk f t :ignore-type ignore-type :with-restrictions with-restrictions)) 
    identity (f term) :ignore-type ignore-type :with-restrictions with-restrictions))


(defn term-prewalk-test
  [term]
  (term-prewalk (fn [x] (print "Walked: ") (prn x) x) term :with-restrictions true))

(defn term-prewalk-test2
  [term]
  (term-prewalk (fn [x] (when (term? x) (print "Walked: ") (prn x)) x) term))

(defn get-antecedents
  [term]
  (let [slot-map (cf/dcsRelationTermsetMap term)]
    (case (type-of term)
      :csneps.core/Conjunction
      (get slot-map (slot/find-slot 'and))
      (:csneps.core/Andor 
       :csneps.core/Disjunction 
       :csneps.core/Xor
       :csneps.core/Nand)
      (get slot-map (slot/find-slot 'andorargs))
      (:csneps.core/Thresh
       :csneps.core/Equivalence)
      (get slot-map (slot/find-slot 'threshargs))
      (:csneps.core/Numericalentailment
       :csneps.core/Implication)
      (get slot-map (slot/find-slot 'ant))
      nil)))

(defn get-vars
  "Returns the vars in the given term, or, if the term is a rule
   returns the intersection of variables in its antecedents. Optionally
   traverses inside variables looking for inner variables."
  [term & {:keys [inner-vars?] :or {inner-vars? false}}]
  (if-let [ants (get-antecedents term)]
    (apply set/intersection (map #(set (filter variable? (flatten-term % :vars? inner-vars?))) ants))
    (set (filter variable? (flatten-term term :vars? inner-vars?)))))
