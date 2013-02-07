; Originally based on the ACL SNePS 3 implementation by Dr. Stuart C. Shapiro.

(in-ns 'csneps.core.build)

(defn apply-sub-to-term
  "Apply a substitution to a term."
  ([term sub] (apply-sub-to-term variable? term sub))
  ([variable? term sub]
   (term-prewalk (fn [x]
                         (if (variable? x)
                           (or (sub x) x)
                           x))
                        term)))

;; Ex: subs1: {arb2: (every x (Isa x Cat)) arb1: (every x (Isa x Entity))}
;;     subs2: {arb1: (every x (Isa x Entity)) cat!}
;;     Result: {arb2: (every x (Isa x Cat)) cat!, arb1: (every x (Isa x Entity)) cat!}
(defn substitution-composition
  "Apples the substitution subs2 to subs1"
  [subs1 subs2] 
  (let [compose (map (fn [[k v]] [k (apply-sub-to-term v subs2)]) subs1)]
    (merge subs2 (into {} compose))))


;;;; OLD VERSION OF THINGS:
(comment 
(defn add-var-term-pair
  [var term subs]
  (assoc subs var term))

(defn add-var-term-pairs
  [var-term-map subs]
  (merge subs) var-term-map)


;"Substitution constructor definition.
;   Substitutions have a hash table that has as a key
;   that is a var and a value that is the terms that
;   can substitute for the var. "
(defrecord2 Substitution
  [table (ref (hash-map))])

;;To make a new substitution, use (new-substitution {}) 
;;and inside the {} but the key/val for table if needed.

(defmethod print-method csneps.core.build.Substitution [o w]
  (.write w (str "{" (apply str
                       (interpose ", "
                         (for [subs @(:table o)]
                           (str (second subs) "/" (first subs))))) "}")))

(defn add-var-term-pair
  [var term subs]
  (dosync (ref-set (:table subs) (assoc @(:table subs) var term)))
  subs)

(defn add-var-term-pairs
  [var-term-map subs]
  (dosync (ref-set (:table subs) (merge @(:table subs) var-term-map)))
  subs)

(defn erase-all-var-term-pairs
  [subs]
  (dosync (ref-set (:table subs) (hash-map)))
  subs)

(defn replace-subs-table
  [subs newtable]
  (erase-all-var-term-pairs subs)
  (add-var-term-pairs newtable subs)
  subs)

(defn size
  [subs]
  (count @(:table subs)))

(defn variables
  [subs]
  (keys @(:table subs)))

(defn empty?
  [subs]
  (= (count @(:table subs)) 0))

(defn var-to-term
  [var subs]
  (get @(:table subs) var))

(defn binds-term
  "Returns the variable the term is bound in this substitution,
   else returns nil."
  [term subs]
  (get term (clojure.set/map-invert @(:table subs))))

(defn equal-bound-term
  "Determines if two substitutions bind the same term,
   regardless of variable. Returns nil if they don't."
  [first second]
  (> (count (clojure.set/intersection (set (vals @(:table first))) (set (vals @(:table second))))) 0))

;; New more efficient implementation [DRS 2/19/12]
(defn copy-substitution
  "Returns a copy of the passed in substitution."
  [subs]
  (new-substitution {:table (ref @(:table subs))}))

;;Prioritizes first over second
(defn join
  "Combines two substitutions into one."
  [first second]
  (conj second first))
)