(in-ns 'csneps.snip)

;;; An S-Index is a map from substitution to a RUI. 
;;; Preconditions for use:
;;;  - Disjunctive rule,
;;;  - Each antecedent uses all the same variables.

(defrecord SIndex
  [sindex ;;A ref to a hash-map.
   matched-msgs
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
  (seen-message?
    [this msg]
    (let [msg (sanitize-message msg)
          old-msg (@(:sindex this) (:subst msg))]
      (= old-msg msg)))
  (get-sent-messages 
    [this chtype] 
    (@(:sent-msgs this) chtype))
  (get-matched-messages
    [this]
    @(:matched-msgs this))
  (add-matched-and-sent-messages
    [this matched sent]
    (dosync 
      (alter (:matched-msgs this) union matched)
      (alter (:sent-msgs this) (partial merge-with union) sent)))
  (print-messages
    [this]
    (println "--- S-Index ---")
    (println "Matched Messages:")
    (doseq [mm @(:matched-msgs this)] (println mm))
    (println "Sent Messages:")
    (doseq [[chtype sms] @(:sent-msgs this)
            sm sms] (println chtype ":" sm))
    (println "Working Messages:")
    (doseq [wm (vals @(:sindex this))] (println wm))))

(defn make-sindex
  []
  (SIndex. (ref {}) (ref {}) (ref {})))