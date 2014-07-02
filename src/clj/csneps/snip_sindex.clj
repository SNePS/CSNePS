(in-ns 'csneps.snip)

;;; An S-Index is a map from substitution to a RUI. 
;;; Preconditions for use:
;;;  - Disjunctive rule,
;;;  - Each antecedent uses all the same variables.

(defrecord SIndex
  [sindex ;;A ref to a hash-map.
   sent-msgs] ;;A ref to a set.
  MessageStructure
  (get-new-messages [this new-msg]
    (let [old-msg (@(:sindex this) (:subst new-msg))]
      (if old-msg
        (let [merged-msg (merge-messages new-msg old-msg)]
          ;; I don't think we need to check compatibility - 
          ;; they can't have different bindings at this point.
          (dosync (alter (:sindex this) assoc (:subst new-msg) merged-msg))
          #{merged-msg})
        (do 
          (dosync (alter (:sindex this) assoc (:subst new-msg) new-msg))
          #{new-msg}))))
  (get-sent-messages 
    [this chtype] 
    (@(:sent-msgs this) chtype))
  (add-matched-and-sent-messages
    [this matched sent]
    (dosync (alter (:sent-msgs this) (partial merge-with union) sent))))

(defn make-sindex
  []
  (SIndex. (ref {}) (ref {})))