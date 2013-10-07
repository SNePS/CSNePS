(in-ns 'csneps.snip)

;;; An S-Index is a map from substitution to a RUI. 
;;; Preconditions for use:
;;;  - Disjunctive rule,
;;;  - Each antecedent uses all the same variables.

(defrecord SIndex
  [sindex] ;;A ref to a hash-map.
  RUIStructure
  (get-rule-use-info [this new-rui]
    (let [old-rui (@(:sindex this) (:subst new-rui))]
      (if old-rui
        (let [merged-rui (merge new-rui old-rui)]
          ;; I don't think we need to check compatibility - 
          ;; they can't have different bindings at this point.
          (dosync (alter (:sindex this) assoc (:subst new-rui) merged-rui))
          #{merged-rui})
        (do 
          (dosync (alter (:sindex this) assoc (:subst new-rui) new-rui))
          #{new-rui})))))

(defn make-sindex
  []
  (SIndex. (ref {})))