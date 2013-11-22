; Originally based on the ACL SNePS 3 implementation by Dr. Stuart C. Shapiro.

(in-ns 'csneps.core.build)

(declare structurally-subsumes-varterm)

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
(defn substitution-application
  "Apples the substitution subs2 to subs1"
  [subs1 subs2] 
  (let [compose (map (fn [[k v]] [k (apply-sub-to-term v subs2)]) subs1)]
    (clojure.core/merge subs2 (into {} compose))))

(defn compatible?
  "Returns true if:
   1) No variable is bound to two different terms.
   2) No term is bound by two different variables."
  [subs1 subs2]
  (and 
    ;; Verify variables aren't bound to different terms.
    (every? #(or (= (% subs1) (% subs2))
                 (nil? (% subs2)))
            (keys subs1))
    ;; Verify terms aren't bound to different variables. (UVBR)
;    (let [inv1 (set/map-invert subs1)
;          inv2 (set/map-invert subs2)]
;      (every? #(or (= (% inv1) (% inv2))
;                   (nil? (% inv2)))
;              (keys inv1)))
    ))

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

(defn subset?
  "Returns true if subs1 is a subset of subs2"
  [subs1 subs2]
  (every? #(= (subs1 %) (subs2 %)) (keys subs1)))

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
  (.write w (str "{" (clojure.string/join
                       ", "
                       (for [subs @(:table o)] (str (second subs) "/" (first subs))))
                 "}")))

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
  (zero? (count @(:table subs))))

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
  (pos? (count (clojure.set/intersection (set (vals @(:table first))) (set (vals @(:table second)))))))

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