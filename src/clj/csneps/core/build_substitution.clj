; Originally based on the ACL SNePS 3 implementation by Dr. Stuart C. Shapiro.

(in-ns 'csneps.core.build)

(declare structurally-subsumes-varterm parse-vars-and-rsts check-and-build-variables notsames pre-build-vars build-vars build-quantterm-channels)

(defn apply-sub-to-term
  ([term subst]
    (apply-sub-to-term term subst nil))
  ([term subst ignore-type]
  (term-prewalk #(subst % %) term :with-restrictions true :ignore-type ignore-type)))

;; Ex: subs1: {arb2: (every x (Isa x Cat)) arb1: (every x (Isa x Entity))}
;;     subs2: {arb1: (every x (Isa x Entity)) cat!}
;;     Result: {arb2: (every x (Isa x Cat)) cat!, arb1: (every x (Isa x Entity)) cat!}
(defn substitution-application
  "Apples the substitution subs2 to subs1"
  [subs1 subs2] 
  (let [compose (map (fn [[k v]] [k (apply-sub-to-term v subs2)]) subs1)]
    (into subs2 compose)))
    ;(clojure.core/merge subs2 (into {} compose))))

;; Ex: subs2: {arb1: (every x (Isa x Cat)) cat}
;;     subs1: {arb2: (every x (Isa x BlahBlah)) arb1: (every x (Isa x Cat))}
;;     Result: {arb2: (every x (Isa x BlahBlah)) cat}
(defn substitution-application-nomerge
  [subs1 subs2]
  (into {} (map (fn [[k v]] [k (apply-sub-to-term v subs2)]) subs1)))

(defn subst-occurs-helper
  "Returns true if when compare-subst contains a substitution for any variable inside var,
   it also binds var, unless there's an identical binding already in var-subst."
  ;; TODO: Must it bind ALL of the inner vars?
  [var var-subs compare-subs] 
  (let [inner-vars (get-vars var :inner-vars? true)
        binds (set (keys compare-subs))
        shared-binds (set/intersection binds inner-vars)]
    (if-not (empty? shared-binds)
      (or (binds var)
          (every? #(= (var-subs %) (compare-subs %)) shared-binds))
      true)))

(defn substitution-occurs-check
  "Ensures that whenever a variable is used in a substitution, the substitution
   it is combined with does not contain bindings for inner variables without a 
   binding for the parent, or, if it does, that the inner variable binding isn't
   identical between the two substitutions."
  [subs1 subs2]
  (and (every? #(subst-occurs-helper % subs2 subs1) (keys subs2))
       (every? #(subst-occurs-helper % subs1 subs2) (keys subs1))))

(defn compatible-substitutions?
  "Returns true if no variable is bound to two different terms."
  [subs1 subs2]
  ;; Verify no single variable is bound to different terms, and 
  ;;    no notSame variables are assigned the same term.
  (and (every? #(or (= (subs1 %) (subs2 %))
                    (nil? (subs2 %)))
               (keys subs1))
       (substitution-occurs-check subs1 subs2)
       (every? true? (for [var (set/union (keys subs1) (keys subs2))
                           :let [notsames @(:not-same-as var)
                                 binding (or (subs1 var) (subs2 var))]]
                       (every? #(not= binding (or (subs1 %) (subs2 %))) notsames)))))

(defn subsumption-compatible?
  "Returns true if:
   1) No variable is bound to two different terms.
   2) A variable is bound by two different terms, of which one of them is
      a variable, and they are compatible by structural subsumption."
  [subs1 subs2]
  (every? #(or (= (% subs1) (% subs2))
               (nil? (% subs2))
               (and (variable? (% subs1)) 
                    (structurally-subsumes-varterm (% subs1) (% subs2)))
               (and (variable? (% subs2)) 
                    (structurally-subsumes-varterm (% subs2) (% subs1))))
          (keys subs1)))

;(defn subset?
;  "Returns true if subs1 is a subset of subs2"
;  [subs1 subs2]
;  (every? #(= (subs1 %) (subs2 %)) (keys subs1)))

(defn expand-substitution
  "Given a term and a substitution, examines the term for embedded vars
   which use terms in the substitution. Builds a new substitution with those
   terms substituted for."
  [term subst]
  (let [term-vars (set/difference (get-vars term) (set (keys subst)))]
    (if-not (empty? term-vars)
      (into {} (map #(vector % (apply-sub-to-term % subst)) term-vars)))))