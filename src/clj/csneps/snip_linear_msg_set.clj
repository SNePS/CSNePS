(in-ns 'csneps.snip)

;; This is the worst case, combinatorial algorithm. 

(defrecord LinearMessageSet
  [msg-set] ;;A ref to a set.
  MessageStructure
  (get-rule-use-info [this new-msg]
    ;; if new-msg isn't actually new, return empty set.
    (dosync
      (if (@(:msg-set this) new-msg)
        #{}
        (let [compat-msgs (filter #(compatible? % new-msg) @(:msg-set this))
              merged-msgs (set (map #(merge-messages new-msg %) compat-msgs))]
          (alter (:msg-set this) union merged-msgs #{new-msg})
          (conj merged-msgs new-msg))))))

(defn make-linear-msg-set
  []
  (LinearMessageSet. (ref #{})))