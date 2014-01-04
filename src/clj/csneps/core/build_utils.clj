(in-ns 'csneps.core.build)

(defn ignore-variable? [sym] (= '_ sym))

(def variable? #(or (= (type-of %) :csneps.core/Arbitrary) (= (type-of %) :csneps.core/QueryVariable)))

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
    (let [p (:print-pattern (:caseframe term))]
      (if (and (seq? (first p)) (= (first (first p)) 'quote))
        (second (first p))
        (:name (first (first (:down-cableset term))))))))

(defn term-walk
  [inner outer termpart]
  (cond
    (molecularTerm? termpart) (outer (build 
                                       (if-let [fsym (or ((type-of termpart) syntype-fsym-map)
                                                         (let [p (:print-pattern (:caseframe termpart))]
                                                           (when (and (seq? (first p)) (= (first (first p)) 'quote))
                                                             (second (first p)))))]
                                         (conj (doall (map inner (:down-cableset termpart))) fsym)
                                         (doall (map inner (:down-cableset termpart))))
                                       :Proposition 
                                       {}))
    (atomicTerm? termpart) (outer termpart)
    (set? termpart) (set (doall (map inner termpart)))
    :else (error (str "Term contains unknown parts (" termpart ")"))))

(defn term-recur
  [inner outer termpart]
  (cond
    (molecularTerm? termpart) (outer (build (conj (doall (map inner (:down-cableset termpart))) (term-predicate termpart)) 
                                        :Proposition 
                                        {}))
    ;(arbitraryTerm? termpart) (outer (build-variable (list 'every (:var-label termpart) (map inner @(:restriction-set termpart))))) 
    (atomicTerm? termpart) (outer termpart)
    (set? termpart) (set (doall (map inner termpart)))
    :else (error (str "Term contains unknown parts (" termpart ")"))))

(defn term-prewalk
  [f term]
  (term-walk (partial term-prewalk f) identity (f term)))

(defn term-prewalk-test
  [term]
  (term-prewalk (fn [x] (print "Walked: ") (prn x) x) term))

(defn term-prewalk-test2
  [term]
  (term-prewalk (fn [x] (when (term? x) (print "Walked: ") (prn x)) x) term))