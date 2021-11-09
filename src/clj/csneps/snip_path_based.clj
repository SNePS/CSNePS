(in-ns 'csneps.snip)

(declare get-tos get-froms build-path-fn)

(defn asserted-members
  "Given a set of terms, return the set
    containing only those that are asserted in the given context"
  [termset ctx]
  ;(println "finding asserted" termset ctx)
  (loop
    [ts termset
     result #{}]
    (cond
      (empty? ts) result
      (ct/asserted? (first ts) ctx) (recur (rest ts)
                                           (conj result (first ts)))
      :else (recur (rest ts)
                   result))))

(defn pb-findfroms
  "Returns the set of nodes
   from which the given slot, or a path for the slot, goes to terms."
  [terms slotname]
  {:pre [(or (set? terms) (term? terms))]}
  (let [termset (if (set? terms) terms (hash-set (get-term terms)))
        slot (slot/find-slot slotname)
        bfn @(:b-pathfn slot)]
    (if bfn 
      (bfn termset)
      (get-froms termset slot))))
    
(defn pb-findtos
  "Returns the set of nodes to which the given slot goes from 
   each of the terms, or to which the path for the slot goes"
  [terms slotname]
  {:pre [(or (set? terms) (term? terms))]}
  (let [termset (if (set? terms) terms (hash-set (get-term terms)))
        slot (slot/find-slot slotname)
        ffn @(:f-pathfn slot)]
    (if ffn 
      (ffn termset)
      (get-tos termset slot))))

(defn get-froms
  "Given a set of nodes and a symbol that names a slot/arc, 
    returns all the nodes that have that arc going from them 
    to one or more of the input nodes"
  [nodes slotname]
  (loop [nodeset nodes
         result #{}]
    (if (empty? nodeset)
      result
      (let [n     (first nodeset)
            froms (find-utils/findfrom n slotname)]
        (if (seq froms)
          (recur (rest nodeset) (union result froms))
          (recur (rest nodeset) result))))))

(defn get-tos
  "Given a set of nodes and a symbol that names a slot/arc, 
    returns all the nodes that have that arc going to them 
    from one or more of the input nodes"
  [nodes slotname]
  (loop [nodeset nodes
         result #{}]
    (if (empty? nodeset)
      result
      (let [n   (first nodeset)
            tos (find-utils/findto n slotname)]
        (if (seq tos)
          (recur (rest nodeset) (union result tos))
          (recur (rest nodeset) result))))))

(defn path-keyword?
  "Returns nil if the argument is not a path keyword"
  [s]
  (get 
    #{'and 'or 'compose 'kstar 'kplus 'not 'relative-complement 
      'irreflexive-restrict 'restrict 'converse}
    s))

(defn rev-slotname
  "Given a symbol that names a slot (either 'forward' or 'backward'):
      if the symbol is the backward name of a slot, return the forward name 
      otherwise, return nil"
  [sym]
  (let [s (str sym)]
    (when (= (.substring s (dec (count s))) "-")
      (symbol (.substring s 0 (dec (count s)))))))

(defn converse
  "Given a path expression, returns its converse"
  [path]
  (when path
    (if (symbol? path)			; unitpath
      (if (= '! path)
        path
        (let [revname (rev-slotname path)]
          (if revname
            revname
            (str path "-"))))
      (if (path-keyword? (first path))
        (if (= (first path) 'restrict)
          path
          (cons (first path)
                (reverse
                  (for [elt (rest path)] (converse elt)))))
        (reverse (for [elt path] (converse elt)))))))

(defn f+
  "Given a nodeset and a function, return the nodeset that results 
    from repeately applying the function to the nodeset one or more times"
  [nodeset fun]
  ;(println "f+" nodeset )
  (loop
    [res (set (fun nodeset))
     retval #{}]
    ;(println "res:" res)
    (if (empty? res) 
      retval
      (recur (difference (set (fun res)) retval) (union retval res)))))

(defn f*
  "Given a nodeset and a function, return the nodeset that results 
  from repeately applying the function to the nodeset zero or more times"
  [nodeset fun]
  ;(println "f*" nodeset)
  (union nodeset (f+ nodeset fun)))

(defn memberOrVar
  "Returns t if either symbol is `?' and termSet is non-empty,
      or if the term named `symbol' is a member of the termSet."
  [symbol termSet]
  (or (and (symbol? symbol)
	         (= symbol '?)
	         (seq termSet))
      (get termSet (get-term symbol))))

(defn build-path-fn
  "Given a path expression, returns the function that will traverse that path"
  [path]
  ;(println "Building: " path)
  (if (seq? path)
    (case (first path)
      compose 
      (let [components (doall (map #(build-path-fn %) (rest path)))]
        (apply comp (reverse components))) ;; compose the functions.
      or 
      (let [components (doall (map #(build-path-fn %) (rest path)))]
        (fn [x] (apply union (map #(% x) components))))
      and 
      (let [components (doall (map #(build-path-fn %) (rest path)))]
        (fn [x] (apply intersection (map #(% x) components))))
      kstar (do 
              (assert (= (count path) 2) "kstar must have only one path argument in path")
              (let [fnsplice (build-path-fn (second path))] (fn [x] (f* x fnsplice))))
      kplus (do
              (assert (= (count path) 2) "kplus must have only one path argument in path")
              (let [fnsplice (build-path-fn (second path))] (fn [x] (f+ x fnsplice))))
      converse (build-path-fn (converse (second path)))
      irreflexive-restrict (let [comp (build-path-fn (second path))]
                             #(difference (set (comp %)) %))
      restrict (do
                 (assert (= (count path) 3) "restrict must have two arguments, a path, and an atomicwft")
                 (let [th (third path)
                       comp (build-path-fn (second path))]
                   (fn [x] (loop [terms x
                                  result #{}]
                             (if (or (empty? terms) (nil? terms))
                               result
                               (let [mov (memberOrVar th (comp (hash-set (first terms))))]
                                 (recur (rest terms)
                                        (if mov (conj result (first terms)) result))))))))
      :else (error (str "Unrecognized path expression operator: " (first path))))
    (if (= '! path); handle asserted
      #(asserted-members % (ct/currentContext))
      (let [rev (rev-slotname path)] ; unit path
        (if rev
          #(get-froms % rev)
          #(get-tos % path))))))

(defn definePath
  "Given a slot name and a path expression, 
   generate the functions that will compute that path and its converse, 
   and store them in the slot."
  [slotname pathexpr]
  (let [fwd (build-path-fn pathexpr)
        bwd (build-path-fn (converse pathexpr))
        aslot (slot/find-slot slotname)]
    (dosync
      (ref-set (:path aslot) pathexpr)
      (ref-set (:b-pathfn aslot) bwd)
      (ref-set (:f-pathfn aslot) fwd))
    nil))

(defn pathsfrom 
  [terms path]
  (let [termset (cond
                  (set? terms) terms
                  (nil? terms) #{}
                  (term? terms) #{terms}
                  (symbol? terms) #{(get-term terms)}
                  (seq? terms) (set (map #(get-term %) terms)))
        builtpath (build-path-fn path)]
    (builtpath termset)))

(defn path-based-derivable
  "If the proposition p is derivable in the given context by path-base-inference,
      return a singleton set of that proposition;
   else return the empty set."
  [p context]
  ;(when @goaltrace
  ;  (cl-format true "~&I will consider using Path-Based inference.~%"))
  (if (isa? (type-of p) :csneps.core/Molecular)
    (let [dcs (:down-cableset p)
          result-sets (map set
                           (apply concat
                                  (map (fn [[slot fillers]]
                                         (map #(pb-findfroms % slot) fillers))
                                       (cf/dcsRelationTermsetMap p))))
          results (when-not (empty? result-sets)
                    (remove #(not (ct/asserted? % context))
                            (apply intersection result-sets)))]
      (if (and (seq results)
               (some #(find/eqfillersets (@down-cableset %) dcs) results))
        (do 
          (assertTrace nil (seq results) p "Path-Based inference" context)
          (hash-set p))
        (hash-set)))
    (hash-set)))