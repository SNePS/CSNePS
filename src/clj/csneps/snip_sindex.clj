(in-ns 'csneps.snip)

;;; An S-Index is a map from substitution to a RUI. 
;;; Preconditions for use:
;;;  - Disjunctive rule,
;;;  - Each antecedent uses all the same variables.

(defrecord SIndex
  [sindex] ;;A ref to a hash-map.
  MessageStructure
  (get-rule-use-info [this new-msg]
    (let [old-msg (@(:sindex this) @(:subst new-msg))]
      (if old-msg
        (let [merged-msg (merge-messages new-msg old-msg)]
          ;; I don't think we need to check compatibility - 
          ;; they can't have different bindings at this point.
          (dosync (alter (:sindex this) assoc (:subst new-msg) merged-msg))
          #{merged-msg})
        (do 
          (dosync (alter (:sindex this) assoc (:subst new-msg) new-msg))
          #{new-msg})))))

(defn make-sindex
  []
  (SIndex. (ref {})))