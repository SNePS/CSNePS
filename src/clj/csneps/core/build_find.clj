(in-ns 'csneps.core.build)

(declare find find-in find-helper)

;; Rewritten 10/12/12 [DRS]
(defn eqfillersets
    "Returns True if the corresponding elements
        of the vector dcs1 and the list dcs2 are
       are either both non-sets,
       or are sets of the same size;
     else returns False."
  [dcs1 dcs2]
  (every? true? ;; every? short circuits, so this is a fine way to do it.
          (for [[s1 s2] (map #(vector %1 %2) (seq dcs1) (seq dcs2))]
            (or (and (set? s1) (set? s2) (= (count s1) (count s2)))
                (and (not (set? s1)) (not (set? s2)))))))

;(defn- find-terms-with-filled-slot
;  "Returns a set of terms for which:
;    - Each term has a slot 'slot' as part of caseframe 'cf'
;    - Each term has 'slot' filled with *at least* the fillers in 'fillers'"
;  [cf slot fillers]
;  (apply clojure.set/intersection 
;    (doall (map (fn [m] (set (filter #(= (:caseframe %) cf) (findfrom m slot)))) fillers))))

;; Clojure's hashing function on maps works by hashing each part. This gets slow
;; for terms which are very 'deep'. This is an (experimental) approach to short
;; circuiting, by only doing these comparisons if necessary.
(defn- find-terms-with-filled-slot
  "Returns a set of terms for which:
    - Each term has a slot 'slot' as part of caseframe 'cf'
    - Each term has 'slot' filled with *at least* the fillers in 'fillers'"
  [cf slot fillers]
  (loop [fs (seq fillers)
         res '()]
    (if (empty? fs)
      (apply clojure.set/intersection (map set res))
      (let [filler (first fs)
            termsfrom (filter #(= (:caseframe %) cf) (findfrom filler slot))]
        (if (empty? termsfrom) 
          #{}
          (recur (rest fs) (conj res termsfrom)))))))
          
;; Rewritten 10/12/12 [DRS]
(defn find-exact
  "If there is a molecular term with the case frame cf
        and the given syntactic type,
        and the down-cableset dcs (represented as a list of sets),
     returns that term;
     else returns nil."
  [syntactic-type cf dcs & {:keys [min max]}]
  (let [result (loop [sfmap (map #(vector %1 %2) (seq (:slots cf)) (seq dcs))
                      res #{}]
                 (if (empty? sfmap)
                   res
                   (let [[slot fillers] (first sfmap)
                         sres (when (seq fillers) (find-terms-with-filled-slot cf slot fillers))]
                     (cond
                       (empty? fillers)
                       (recur (rest sfmap)
                              res)
                       (empty? sres)
                       sres
                       :else
                       (recur (rest sfmap)
                              (if (empty? res) sres (clojure.set/intersection res sres)))))))]
    
    (if (nil? result)
      nil
      ;; result is now a set of nodes that might have filler sets that are too large
      (let [ret (for [n result :when (and (is-syntactic-type? (:type n))
                                          (or (nil? min)
                                              (= min (:min n)))
                                          (or (nil? max)
                                              (= max (:max n)))
                                          (eqfillersets (:down-cableset n) dcs))]
                  n)]
        (when (pos? (count ret)) (first ret))))))

;;; Rewritten 6/21/2012 [DRS]
(defn contains-new-term-or-cf?
  "Returns true if a restriction contains new terms not already in the KB."
  [var-label restriction]
  (loop [arg (rest restriction)]
    (cond 
      (empty? arg)        nil
      (list? (first arg)) (contains-new-term-or-cf? var-label (first arg))
      (not (or (= var-label (first arg))
               (= (syntactic-type-of (first arg)) :csneps.core/Term)
               (get-term (first arg))))
                          true
      :else               (recur (rest arg)))))

(defn var-requires-new-term?
  "Returns true if resrictions contain new terms not already in the KB."
  [var-label restrictions]
  (loop [rst restrictions]
    (cond 
      (empty? rst)                                     nil
      (contains-new-term-or-cf? var-label (first rst)) true
      :else                                            (recur (rest rst)))))

(defn find-vars-of-restriction-set-size
  "Returns all vars of type [quant] in the KB which have a restriction set of size [size]."
  [size quant]
  (set (filter #(= (count @(:restriction-set %)) size) 
               (case quant
                 :every @ARBITRARIES
                 :some @INDEFINITES
                 :qvar @QVARS))))
  
(defn get-terms-from-find-results
  "Given results from a find operation, return all of the terms in the substitutions."
  [findres]
  (set (map #(val (first %)) (map second findres))))

(defn find-old-var-node
  "If an existing variable node can be found such that:
      it is of the proper variable type;
      it is of the given semantic type;
      it has the given restrictions,
         where var-label stands for the variable node label in the
         restrictions;
   that existing node is returned.
   Otherwise nil is returned."
  [var-label restrictions arb-rsts ind-dep-rsts qvar-rsts quant notsames]
  (let [distinctres (distinct restrictions)
        var-list (concat (keys arb-rsts) (keys ind-dep-rsts) (keys qvar-rsts))]
    ;; If the variable contains new terms which haven't yet been built,
    ;; we can quit now. Otherwise, find the variables which have the 
    ;; same size as the one we're testing against, then intersect
    ;; that set with the terms in the substitutions resulting from 
    ;; using "find" to look for each of the restrictions of the arb.
    (when-not (var-requires-new-term? var-label distinctres)
      (let [possibles (loop [rst distinctres
                             possibles (find-vars-of-restriction-set-size (count distinctres) quant)]
                        (cond
                          (empty? rst) possibles
                          (empty? possibles) nil
                          :else (recur (rest rst)
                                       (clojure.set/intersection possibles
                                                                 (apply clojure.set/union
                                                                        (map #(get-terms-from-find-results
                                                                                (find-in (first rst) @(:restriction-set %) var-list))
                                                                             possibles))))))
            nsct (count (notsames var-label))]
        (when possibles
          ;; If we can find one of the possibles with the same number of notsames,
          ;; then return that one. Otherwise, return nil.
          (first (filter #(= nsct (count @(:not-same-as %))) possibles)))))))

(defn filter-termset
  [termset expr cf var-list]
  (loop [slots (:slots cf)
         subexs (if (cf/quotedpp? cf) (rest expr) expr)
         tset termset]
    ;(println tset (first subexs) (when (get-term (first subexs)) (findfrom (get-term (first subexs)) (first slots))))
    (if (or 
          (empty? tset)
          (empty? slots)
          (empty? subexs))
      tset
      (recur 
        (rest slots)
        (rest subexs)
        (let [subex (first subexs)]
          (if (and (symbol? subex)
                   (not (some #{subex} var-list))
                   (not (synvariable? subex))
                   (get-term subex))
            (clojure.set/intersection tset (findfrom (get-term subex) (first slots)))
            tset))))))

(declare pattern-term-match)

(defn pattern-term-set-match
  [expr-set term-set var-list subs]
  ;; First, go through expr-set looking for non-variables.
  ;; We can verify their match with something in term-set
  ;; and stop looking at them. 
  (let [vars (set (filter #(or (some #{%} var-list) (synvariable? %)) expr-set))
        non-vars (clojure.set/difference expr-set vars)
        ground-terms (set (map #(get-term %) non-vars))]
    ;; Check that all non-var terms are in the KB.
    (when (= (count ground-terms) (count (filter identity ground-terms)))
      ;; Check that any bound vars can be bound correctly.
      (let [bound-vars (filter #(% subs) vars)
            good-bound-vars (set (filter identity (map #(some #{(% subs)} ground-terms) bound-vars)))
            remaining-vars (clojure.set/difference vars good-bound-vars)
            remaining-terms (clojure.set/difference term-set ground-terms (set (map #(subs %) good-bound-vars)))]
        (when (= (count bound-vars) (count good-bound-vars))
          ;; Now all that's left is to get all combinations of remaining-terms, and combine them with the remaining-vars.
          (let [term-permute (cb/permutations remaining-terms)]
            (map #(into (zipmap remaining-vars %) subs) term-permute)))))))

(defn match-expr-to-term
  [expr term var-list subs]
  (cond 
    ;; Check if expr is a set. If it is, make sure each of its elements
    ;; matches something in the dc.
    (and (set? expr) (= (count expr) (count term)))
    (pattern-term-set-match expr term var-list subs)
    (and (seq? expr) (= (first expr) 'setof) (= (count term) (- (count expr) 1)))
    (pattern-term-set-match (set (rest expr)) term var-list subs)
    ;; Check if expr is a seq. If it is, we'll need to compare it with the
    ;; dcs of the term we're comparing against.
    (seq? expr)
    (pattern-term-match expr (:down-cableset term) var-list subs)
    ;; Check if expr is a variable. If so, we need to verify that any new
    ;; substitution we create will be valid.
    (or (some #{expr} var-list)
        (synvariable? expr))
    (if (expr subs)
      (when (= (expr subs) term)
        subs)
      (assoc subs expr term))
    ;; Otherwise, expr (once built) should be the same as term (the case 
    ;; for expr being a caseframe is handled in pattern-term-match).
    :else
    (if (set? term)
      (when (and (get-term expr) (some #{(get-term expr)} term))
        subs)
      (when (= (get-term expr) term)
        subs))))

(defn match-expr-seq-to-dcs
  [expr dcs var-list subs]
  (cond 
    (and (empty? expr) (empty? dcs))
    subs
    (or (empty? expr) (empty? dcs))
    nil
    :else
    (let [nsubs (match-expr-to-term (first expr) (if (= (count (first dcs)) 1) (first (first dcs)) (first dcs)) var-list subs)]
      (when-not (nil? nsubs)
        (if (seq? nsubs)
          (filter identity (for [sub nsubs] (match-expr-seq-to-dcs (rest expr) (rest dcs) var-list sub)))
          (match-expr-seq-to-dcs (rest expr) (rest dcs) var-list nsubs))))))

(defn pattern-term-match
  [expr dcs var-list subs]
  (let [cf (cf/find-frame (first expr))]
    (cond 
      (and cf (cf/quotedpp? cf))
      (match-expr-seq-to-dcs (rest expr) dcs var-list subs)
      (= (get-term (first expr)) (first (first dcs)))
      (match-expr-seq-to-dcs (rest expr) (rest dcs) var-list subs))))


(defn find-in
  [expr termset var-list]
  (remove #(nil? %)
    (for [term termset]
      (let [subs (pattern-term-match expr (:down-cableset term) var-list {})]
        ;; TODO: Why is this like this? Very odd...
        (cond
          (seq subs) ;; non-empty sequence.
          [term subs]
          (and (not (seq? subs)) subs)
          [term subs])))))


(defn find
  [expr & [var-list]]
  (let [cf (cf/find-frame (first expr))]
    (if cf
      (find-in expr (filter-termset @(:terms cf) expr cf var-list) var-list)
      '())))


(defn postorder-seq-traversal
  [seq]
  (loop [s seq
         out []]
    (if (empty? s) 
      out
      (recur
        (rest s)
        (conj (if (seq? (first s)) (into out (postorder-seq-traversal (first s))) out)
              (first s))))))

(defn check-and-bind-var
  [subs var node]
  (cond 
    (nil? (subs var))                [(assoc subs var (:acceptWft node)) node]
    (= (subs var) (:acceptWft node)) [subs node]
    :else                            nil))

(defn create-var-binds
  [subs var currnode target varfn]
  (let [wftnodes (getWftNodesFor currnode)
        subnodepairs (map #(check-and-bind-var subs var %) wftnodes)
        filteredsnp (filter #(not (nil? %)) subnodepairs)]
    ;(println "Finding " wftnodes) 
    (doall
      (for [snp filteredsnp
              :let [sub (first snp)
                    ch @(:children (second snp))]]
        (doall (map #(find-helper 
                    (rest target)
                    :varfn varfn :subs sub :sourcenode (second %))
                ch))))))

(defn find-helper
  [target & {:keys [varfn subs sourcenode] :or {subs {}}}]
  (let [targetexpr (first target)]
    ;(println targetexpr subs (:acceptWft sourcenode))
    ;(println (= (str targetexpr) (str (:acceptString sourcenode))))
    (cond 
      (and (nil? targetexpr)
           (empty? @(:children sourcenode)))           {(:acceptWft sourcenode) subs}
      (or (nil? sourcenode) (nil? subs))               nil
      (distnode? sourcenode)                           (map #(find-helper
                                                               (rest target)
                                                               :varfn varfn :subs subs :sourcenode %)
                                                         (vals @(:children sourcenode)))
      (and (molecularTerm? (:acceptWft sourcenode))    
           (seq? targetexpr))                          (map #(find-helper
                                                               (rest target)
                                                               :varfn varfn :subs subs :sourcenode %)
                                                         (vals @(:children sourcenode)))
      (varfn targetexpr)                               (create-var-binds subs targetexpr sourcenode target varfn)
      (= (str targetexpr) 
         (str (:acceptString sourcenode)))             (map #(find-helper
                                                               (rest target)
                                                               :varfn varfn :subs subs :sourcenode %)
                                                         (vals @(:children sourcenode)))
      :else                                            nil)))

(defn find-old 
  [expr & var-list]
  (or (apply merge
        (filter #(not (nil? %))
          (flatten
            (find-helper (postorder-seq-traversal expr) 
                         :varfn (if var-list (varinlist? (flatten var-list)) synvariable?)
                         :sourcenode (findDistNode (first expr) (dec (count expr)) @DistNodes)))))
      {}))