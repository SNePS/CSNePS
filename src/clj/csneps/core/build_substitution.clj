; Originally based on the ACL SNePS 3 implementation by Dr. Stuart C. Shapiro.

(in-ns 'csneps.core.build)

(declare structurally-subsumes-varterm)

(defn apply-sub-to-term
  "Apply a substitution to a term."
  ([term sub] (apply-sub-to-term variable? term sub nil))
  ([term sub ignore-type] (apply-sub-to-term variable? term sub ignore-type))
  ([variable? term sub ignore-type]
    (term-prewalk (fn [x]
                          (if-let [s (sub x)]
                            s
                            x))
                         term
                         :ignore-type ignore-type)))

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


(defn compatible-substitutions?
  "Returns true if no variable is bound to two different terms."
  [subs1 subs2]
  ;; Verify variables aren't bound to different terms.
  (every? #(or (= (subs1 %) (subs2 %))
               (nil? (subs2 %)))
          (keys subs1)))

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