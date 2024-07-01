;;; snip/unification.cl
;;; SNePS 3 Unification Algorithm
;;; Daniel R. Schlegel
;;; Standard unification based on a cross between that from clojure.core.unify, and
;;; Dr. Stuart C. Shapiro's CSE563 notes.
;;; Modified: 6/19/2012

(in-ns 'csneps.core.build)

(def debug-unification false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unification Algorithm ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare garner-unifiers unifySets)

(defn occurs?
  "Basic occurs check"
  [var term]
    (some #{var} (flatten term)))

(defn notSameOK?
  "If neither sbinds nor tbinds bind expr to a term which
   is explicitly not the same as qterm, then return true."
  [[sbinds tbinds] qterm expr]
  (if (and (variable? expr)
           (or (@(:not-same-as qterm) expr)
               (@(:not-same-as expr) qterm)))
    false
    (let [expr-binders-sbinds (filter #(= (second %) expr) sbinds)
          expr-binders-tbinds (filter #(= (second %) expr) tbinds)
          expr-binders (into (map first expr-binders-sbinds) 
                             (map first expr-binders-tbinds))
          qtnsa @(:not-same-as qterm) ]
      (loop [binders expr-binders]
        (cond 
          (empty? binders) true
          (or (contains? qtnsa (first binders))
              (contains? @(:not-same-as (first binders)) qterm))
          false
          :else (recur (rest binders)))))))

(defn- bind-phase
  [[source target] variable expr index]
  (if (ignore-variable? variable)
    [source target]
    (if (zero? index)
      [(assoc source variable expr) target]
      [source (assoc target variable expr)])))

(defn- determine-occursness [want-occurs? variable? v expr binds]
  (if want-occurs?
    `(if (occurs? ~v ~expr)
       nil
       (bind-phase ~binds ~v ~expr))
    `(bind-phase ~binds ~v ~expr)))

;;; Set version.
;; Index is either 0 or 1 - 0 for target, 1 for source.
(defn- unify-variable-1binds
  [variable? v expr binds index]
  (if-let [vb# ((binds index) v)]
    (garner-unifiers variable? vb# expr binds)
    (if-let [vexpr# (and (variable? expr) ((binds index) expr))]
      (garner-unifiers variable? v vexpr# binds)
      (when (and (not (occurs? v expr)) (notSameOK? binds v expr)) 
        (bind-phase binds v expr index)))))

(defn- unify-variable
  [variable? v expr binds index]
  ;(println (unify-variable-1binds varp v expr binds index))
  ;(println "v: " v "expr: " expr "binds: " (apply str binds) "index: " index)
  (if (list? binds)
    (remove nil?
      (for [b binds]
        (unify-variable-1binds variable? v expr b index)))
    (unify-variable-1binds variable? v expr binds index)))

(defn- garner-unifiers
  "Attempt to unify x and y with the given bindings (if any). Potentially returns a map of the
   unifiers (bindings) found.  Will throw an `IllegalStateException` if the expressions
   contain a cycle relationship.  Will also throw an `IllegalArgumentException` if the
   sub-expressions clash."
  ([s t]           (garner-unifiers variable? s t))
  ([variable? s t] (garner-unifiers variable? s t [{} {}]))
  ([variable? s t binds] (garner-unifiers unify-variable variable? s t binds))
  ([uv-fn variable? s t [source target]]
    ;(println "#U# Source:" s "Source Bindings:" source "Target:" t "Target Bindings:" target)
     (cond
       (not (and source target)) [nil nil]
       (= (:name s) (:name t))                   [source target]
       (set? s)                  (let [unif (unifySets s (if (set? t) t #{t}) [source target] 0)]
                                   (when debug-unification (println "***" (unifySets s (if (set? t) t #{t}) [source target] 0)))
                                   unif)
       (set? t)                  (let [unif (unifySets t (if (set? s) s #{s}) [source target] 1)]
                                   (when debug-unification (println "***" (unifySets t (if (set? s) s #{s}) [source target] 1)))
                                   unif)
       (variable? s)             (uv-fn variable? s t [source target] 0)
       (variable? t)             (uv-fn variable? t s [source target] 1)
       (and (molecularTerm? s)
            (molecularTerm? t))  (garner-unifiers variable?
                                                 (@down-cableset s)
                                                 (@down-cableset t)
                                                 (garner-unifiers variable?
                                                                  (term-predicate s)
                                                                  (term-predicate t)
                                                                  [source target]))
       :else nil)))

(defn- subst-bindings-1bind
  "Flattens recursive bindings in the given map."
  [variable? [source target]]
  (let [substfn (fn [[k v]]
                   [k (loop [v v]
                        (if (and (variable? v) (or (source v) (target v)))
                          (recur (or (source v) (target v)))
                          v))])
        s (into {} (map substfn source))
        t (into {} (map substfn target))]
    [s t]))

(defn- subst-bindings
  ([binds] (subst-bindings variable? binds))
  ([variable? binds]
    ;(println "SB" binds)
    (if (seq? binds)
      (for [b binds]
        (subst-bindings-1bind variable? b))
      (subst-bindings-1bind variable? binds))))

(defn- try-subst-map-1bind
  "Attempts to substitute the bindings in the appropriate locations in the given source/target bindings maps."
  [variable? [source target]]
  ;(println "substmap" source target)
  (let [substfn (fn [set1 set2]
                  (zipmap (keys set1)
                    (for [[k x] set1]
                      (term-prewalk (fn [expr]
                                      (if (variable? expr)
                                        (or (set1 expr) (set2 expr) expr)
                                        expr))
                        x))))]
    [(substfn source target) (substfn target source)]))

(defn- try-subst-map
  [variable? binds]
  (if (seq? binds)
    (for [b binds]
      (try-subst-map-1bind variable? b))
    (try-subst-map-1bind variable? binds)))

(defn unify
  "Attemps the unification process from garnering the bindings to producing a
    final set of substitutions."
  ([x y] (unify variable? x y))
  ([variable? x y]
     (unify variable? x y (garner-unifiers variable? x y)))
  ([variable? x y binds]
;    (println "Unifying: " x " and " y)
    (when binds
      (->> binds
           (subst-bindings variable?)
           (try-subst-map variable?)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additions for Unification of Sets ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Permute subset takes as input a desired length of the permutations, and a set.
(defn permute-subset [n xs]
  (if (= 1 n)
    (map list xs)
    (reduce
      #(let [s (split-at (inc %2) xs)
             ps (permute-subset (dec n) (last s))
             a (last (first s))]
         (into %1 (map (fn [p] (cons a p)) ps)))
      '() (range 0 (inc (- (count xs) n))))))

(defn findSetUnifiers [set1 set2]
  (for [x set1
        y set2]
    (garner-unifiers x y)))

;(defn reduction [binds permutation]
;  (reduce
;    #(unify-variable variable? (ffirst %2) (second (first %2)) %1)
;    binds
;    permutation))

(defn extract-permutation [rows perm]
  "Takes a list of rows, and a permutation value, and returns the proper items
    from the rows. For example:
    (extract-permutation '([{?x ?z} {?x Alex}] [{?z (caregiver ?y)} nil]) [1 0])
    ({?x Alex} {?z (caregiver ?y)})"
  (for [p (map #(vector %1 %2) perm rows)]
    (nth (second p) (first p))))

;; [[{} {arb1: (every x ) Lassie}]]
;; to:
;; [{arb1: (every x ) Lassie}
;; with idx = 1
(defn getbinds
  [unifier idx]
  (mapv #(nth % idx) unifier))

;;; The Set-Based Unification Process
;;; Illustrated with the example: #{'?x '?y '(caregiver ?y)} (source) unified with #{'?z 'Alex} (target)
;;; 1) Find the unifiers for each term in the target set unified with each term
;;;    in the source set. This is done in findSetUnifiers.
;;;    Result:
;;;    [[{?y ?z} {?y Alex}]
;;;     [{?x ?z} {?x Alex}]
;;;     [{?z (caregiver ?y)} nil]]
;;; 2) Find permutations of size (count target) of the result of set 1. That is,
;;;    we're selecting every permutation of rows in the above table. This is
;;;    done using permute-subset.
;;;    Result:
;;;    (([{?y ?z} {?y Alex}] [{?x ?z} {?x Alex}])
;;;     ([{?y ?z} {?y Alex}] [{?z (caregiver ?y)} nil])
;;;     ([{?x ?z} {?x Alex}] [{?z (caregiver ?y)} nil]))
;;; 3) Now we have (count source) different row permutations. For each of these, we
;;;    need to find (count target) available combinations of substitutions.
;;;    We generate the available permutations by calling extract-permutations.
;;;    Result:
;;;    (({?y ?z} {?x Alex})
;;;     ({?y Alex} {?x ?z})
;;;     ({?y ?z} nil)
;;;     ({?y Alex} {?z (caregiver ?y)})
;;;     ({?x ?z} nil)
;;;     ({?x Alex} {?z (caregiver ?y)}))
;;; 4) We can safely ignore any of the above which have a "nil" list entry.
;;;    For the ones which remain, we combine the unifiers for the variables,
;;;    and return the valid results.
;;;    Result:
;;;    ({?x Alex, ?y ?z}
;;;     {?x ?z, ?y Alex}
;;;     {?z (caregiver ?y), ?y Alex}
;;;     {?z (caregiver ?y), ?x Alex})
;;; Note that the resolution of variables in these is handled later.

(defn unifySets [set1 set2 binds idx & {:keys [varfn] :or {varfn variable?}}]
  ;(println "Unifying sets: " set1 "\nand: \n" set2) 
  ;; There are clearly bugs here. Just quit if there's no variables.
  (if (and (= (filter variable? set1) '()) (= (filter variable? set2) '()) (not= set1 set2))
    nil
    (let [unifiers (set/difference (set (findSetUnifiers set1 set2)) #{[nil nil] nil}) ;; Step 1.
          unifpermute (permute-subset (count set2) unifiers) ;; Step 2.
          reduction (fn [permutation]
                      ;(println "!!!" (ffirst permutation) (second (first permutation)))
                      (let [binds (if (seq (ffirst permutation))
                                    (reduce 
                                      #(unify-variable varfn (first (ffirst %2)) (second (ffirst %2)) %1 0)
                                      binds
                                      permutation)
                                    binds)]
                        (if (seq (second (first permutation)))
                          (reduce 
                            #(unify-variable varfn (ffirst (second %2)) (second (first (second %2))) %1 1)
                            binds
                            permutation)
                          binds)))]
      ;(binding [*print-level* 6] 
      ;  (println "Set Unifiers " unifiers)
      ;  (println "Permutations" unifpermute)) 
      (remove nil? 
              (loop [unifpermute unifpermute
                     sub (map #(extract-permutation (first unifpermute) %) 
                              (cb/permutations (range (count (first unifpermute)))))
                     result []]
                (cond 
                  (and (empty? sub)
                       (<= (count unifpermute) 1)) result
                  (empty? sub) (recur (rest unifpermute)
                                      (map #(extract-permutation (first (rest unifpermute)) %) 
                                           (cb/permutations (range (count (first (rest unifpermute))))))
                                      result)
                  (not (some nil? (flatten sub))) (recur unifpermute
                                                         (rest sub)
                                                         (conj result (reduction (first sub))))
                  :else (recur unifpermute
                               (rest sub)
                               result)))))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tree Unification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(declare unifyTreeWithChain)

;(defn molecular? [term]
;  (isa? (csneps/syntactic-type-of term) :csneps.core/Molecular))



(defn- treeUnifyVar
  [variable? sourcenode targetnode [s t]]
  ;(println "Unifying: " (:acceptWft sourcenode) [s t] (:acceptWft targetnode) (garner-unifiers variable? (:acceptWft sourcenode) (:acceptWft targetnode) [s t]))
  (let [unifiers (garner-unifiers variable? (:acceptWft sourcenode) (:acceptWft targetnode) [s t])]
    (if (seq? unifiers)
      (for [u unifiers]
        [sourcenode targetnode u])
      (list [sourcenode targetnode unifiers]))))

(defn- treeUnifyVarList
  "Takes a variable wft and a list of nodes representing wfts to unify the variable
   with. It also takes a source and target binding, along with index representing
   whether or not the result goes in the source or target. 0 = variable is source, 1 = x is source "
  [variable? sourcenodelist targetnode [s t]]
  ;(println "Unify var list..." (for [sn sourcenodelist] (treeUnifyVar variable? sn targetnode [s t])))
  (let [unifiers (apply concat (for [sn sourcenodelist] (treeUnifyVar variable? sn targetnode [s t])))]
    ;(binding [*print-level* 4] 
    ;  (println unifiers))
    (doall 
      (for [[sn tn [sb tb]] unifiers
            :let [tn-children @(:children tn)]]
        (map #(unifyTreeWithChain tn-children :variable? variable? :s sb :t tb :source %) (vals @(:children sn)))))))

(defn unifyVarTree
  [variable? sourcenode targetnode [s t]]
  (treeUnifyVarList variable? (getWftNodesFor sourcenode) (first (getWftNodesFor targetnode)) [s t]))

(defn unifyTreeWithChain
  [target & {:keys [variable? s t source distnodes] :or {variable? variable?, s {}, t {}, source true, distnodes @DistNodes}}]
  (let [targetnode (second (first target))
        sourcenode (if (distnode? targetnode)
                     (findDistNode (:name targetnode) (:arity targetnode) distnodes)
                     source)
        target-children (when (:children targetnode) @(:children targetnode))]
    ;(binding [*print-level* 4] 
    ;  (println "Source: " sourcenode "\nTarget: " targetnode)
    ;  (println "Source Binding:" s "\nTarget Binding:" t))
    (cond
      (not (and s t))                                 [nil nil]
      (and (nil? sourcenode) (nil? targetnode))       [s t]
      (distnode? sourcenode)                          (map #(unifyTreeWithChain
                                                              target-children
                                                              :variable? variable? :s s :t t :source %
                                                              :distnodes distnodes)
                                                        (vals @(:children sourcenode)))
      (and (:name (:acceptWft sourcenode))
           (:name (:acceptWft targetnode))
           (= (:name (:acceptWft sourcenode))
              (:name (:acceptWft targetnode))))       (map #(unifyTreeWithChain
                                                              target-children
                                                              :variable? variable? :s s :t t :source %
                                                              :distnodes distnodes)
                                                               (vals @(:children sourcenode)))
      (and (molecularTerm? (:acceptWft sourcenode))
           (molecularTerm? (:acceptWft targetnode)))  (if (or (not-empty @(:children sourcenode))
                                                              (not-empty target-children))
                                                        (map #(unifyTreeWithChain
                                                                target-children
                                                                :variable? variable? :s s :t t :source %
                                                                :distnodes distnodes)
                                                          (vals @(:children sourcenode)))
                                                        {:source (:acceptWft sourcenode) 
                                                         :target (:acceptWft targetnode) 
                                                         :sourcebind s 
                                                         :targetbind t})
      ;; Variables.
      (or 
        (variable? (:acceptWft sourcenode))           
        (variable? (:acceptWft targetnode)))          (unifyVarTree variable? sourcenode targetnode [s t])
      (or 
        (set? (:acceptWft sourcenode))
        (set? (:acceptWft targetnode)))               (let [setunifres (unifySets 
                                                                         (if (set? (:acceptWft sourcenode))
                                                                           (:acceptWft sourcenode)
                                                                           #{(:acceptWft sourcenode)})
                                                                         (if (set? (:acceptWft targetnode))
                                                                           (:acceptWft targetnode)
                                                                           #{(:acceptWft targetnode)})
                                                                         [s t] 0)]
                                                        (for [[sb tb] setunifres
                                                              source (vals @(:children sourcenode))]
                                                          (unifyTreeWithChain
                                                            target-children
                                                            :variable variable? :s sb :t tb :source source
                                                            :distnodes distnodes)))
      :else nil)))

(defn getUnifiers
  [term & {:keys [varfn distnodes] :or {varfn variable? distnodes @DistNodes}}]
  (let [tempdist (ref {})
        _ (addTermToUnificationTree term :distnodes tempdist)
        tempdist @tempdist
        unifiers (when (findDistNode (:name (second (first tempdist))) (:arity (second (first tempdist))) distnodes)
                   (remove nil? (flatten (unifyTreeWithChain tempdist :variable? varfn :distnodes distnodes))))]
    (for [u unifiers
          :let [[subsourcebind subtargetbind] (->> [(:sourcebind u) (:targetbind u)]
                                                (subst-bindings variable?)
                                                (try-subst-map variable?))]]
      {:source (:source u) :target (:target u) :sourcebind subsourcebind :targetbind subtargetbind})))


(defn- vvbindreverse
  [[t1 t2]]
  (when (and (variable? t1) (variable? t2))
    [t2 t1]))

;; Variable pairs for variable changes need to go in the switch, and not in the filter.
(defn- fix-binds
  [unifier]
  (let [sbinds (into (:sourcebind unifier) (map vvbindreverse (:targetbind unifier)))
        tbinds (into {} (filter #(not (and (variable? (first %)) (variable? (second %)))) (:targetbind unifier)))]
    (-> unifier (assoc :sourcebind sbinds) (assoc :targetbind tbinds))))

(defn- check-binding-matches
  "The actual check that v1 is a subtype (or equal type) of t1, and that if t1 is a 
   variable, that it subsumes v1"
  [[v1 t1]]
  ;; Check that t1 can replace v1 legally. That is, all v1's are t1's.
  ;; Allow for cases where semantic types don't match now but *could* in the future. 
  ;; Bad types will be blocked in the channels, so this is OK, and we don't want to
  ;; not build channels which could become relevant. (Same idea used in match-single-unifier)
  (if (gcsubtype (st/semantic-type-of v1) (st/semantic-type-of t1))
    (if (variable? t1)
      (subsumes? t1 v1)
      true)
    nil))
  
(defn- match-single-unifier
  ""
  [unifier]
  (let [unifier (fix-binds unifier)
        sourcebind (:sourcebind unifier)
        targetbind (:targetbind unifier)
        sbok (every? check-binding-matches sourcebind)
        tbok (and (every? check-binding-matches targetbind)
                  ;; Don't build channels if there's no hope of the semantic types working out in the future. 
                  ;; We recognize that lowering can happen, so Entity -> Category is OK, but we don't want
                  ;; something like Category -> Policy. Some of these will be built if a term started as Entity
                  ;; and was later lowered to Category, but let's not pollute things by always building them. 
                  ;; Note messages like Entity -> Category are being stopped in the channels. 
                  (every? (fn [[v1 v2]] (gcsubtype (st/semantic-type-of v2) (st/semantic-type-of v1))) sourcebind))]
    ;(println unifier)
    ;(println sourcebind "\n" targetbind "\n" sbok tbok)
    (cond
      (and sbok tbok) (list unifier {:source (:target unifier)
                                     :target (:source unifier)
                                     :sourcebind targetbind
                                     :targetbind sourcebind})
      sbok (list unifier)
      tbok (list {:source (:target unifier)
                  :target (:source unifier)
                  :sourcebind targetbind
                  :targetbind sourcebind})
      :else nil)))

(defn clean-matches
  "Removes ind/ind from source and target bindings. These bindings were needed for 
   subsumption testing, but now that that's done they can go."
  [match]
  (let [clean-binds (fn [binds] 
                      (into {} (for [[k v] binds
                                     :when (not (and (indefiniteTerm? k) (indefiniteTerm? v)))]
                                 [k v])))]
    (for [m match]
      (-> m 
        (assoc :targetbind (clean-binds (:targetbind m)))
        (assoc :sourcebind (clean-binds (:sourcebind m)))))))

(defn match
  "Given a term, matches the term against the unificationt tree, and returns
   the set of unifiers along with bindings which unify, satisfy type constraints,
   and satisfy subsumption constraints. If term may be both the source and target
   of a unified term, both versions are returned."
  [term]
  (let [unifiers (getUnifiers term)
        matches (apply concat (map match-single-unifier unifiers))]
    ;(println "UNIFIERS" unifiers)
    ;(println "MATCHES" (clean-matches matches))
    (clean-matches matches)))

(defn unify?
  "Returns the unifiers if term1 unifies with term2. Creates a temporary
    term tree for term1, and then calls getUnifiers."
  [term1 term2 & {:keys [varfn] :or {varfn variable?}}]
  (let [term1-tempdist (ref {})
        term1-chain (addTermToUnificationTree term1 :distnodes term1-tempdist)]
    (getUnifiers term2 :distnodes @term1-tempdist)))
      
      
    
    ;(if unifiers
    ;  (->> unifiers
    ;       (subst-bindings variable?)
    ;       (try-subst-map variable?))len
    ;  nil)
    ;unifiers))

;(defn test []
;  (def t1 (snuser/assert '(Isa Glacier Cat)))
;  (def t2 (snuser/assert '(Isa Glacier Animal)))
;  (addTermToUnificationTree t1)
;  (addTermToUnificationTree t2)
;  (def t3 (snuser/assert '(Isa (every x) Animal)))
;  (getUnifiers t3))

(defn reset-tree
  []
  (dosync (ref-set DistNodes {})))

;;;;;;;;;;;;;;;;
;;; Generics ;;;
;;;;;;;;;;;;;;;;

(defn satisfies-quantterm?
  "If term satisfies each restriction on
    arb, then term satisfies arb."
  [term arb]
  (when (and term arb)
    (let [rs (@restriction-set arb)]
      (every? #(not (nil? %)) 
              (map #(ct/asserted? 
                      (apply-sub-to-term % {arb term}) 
                      (ct/currentContext)) rs)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diagnostic Tools ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-tree [node & {:keys [depth] :or {depth 0}}]
  (when node
    (let [currnode (if (map? node) (second (first node)) (second node))]
      (println (apply str (repeat depth "-")) (if (:acceptWft currnode)
                                                (csneps.core.printer/term-printer (:acceptWft currnode))
                                                (or (:name currnode) (:acceptString currnode))))
      (doseq [c @(:children currnode)]
        (print-tree c :depth (inc depth))))))

(defn print-all-trees []
  (doseq [d @DistNodes] (print-tree d))) 
