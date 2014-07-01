(in-ns 'csneps.snip)

;; This is the worst case, combinatorial algorithm. 

(defrecord LinearMessageSet
  [msg-set] ;;A ref to a set.
  MessageStructure
  (get-rule-use-info [this new-msg]
    ;; if new-msg isn't actually new, return empty set.
    (let [new-msg (sanitize-message new-msg)]
      (dosync
        (if (@(:msg-set this) new-msg)
          #{}
          (let [compat-msgs (filter #(compatible? % new-msg) @(:msg-set this))
                merged-msgs (map #(merge-messages new-msg %) compat-msgs)
                new-merged-msgs (set (filter #(not (@(:msg-set this) %)) merged-msgs))]
            (alter (:msg-set this) union new-merged-msgs #{new-msg})
            (conj new-merged-msgs new-msg)))))))

(defn make-linear-msg-set
  []
  (LinearMessageSet. (ref #{})))