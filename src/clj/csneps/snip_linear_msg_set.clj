(in-ns 'csneps.snip)

;; This is the worst case, combinatorial algorithm. 

(defrecord LinearMessageSet
  [matched-msgs
   working-msgs
   sent-msgs] ;;A ref to a set.
  MessageStructure
  (get-new-messages [this new-msg]
    ;; if new-msg isn't actually new, return empty set.
    (let [new-msg (sanitize-message new-msg)]
      (dosync
        (if (or (@(:working-msgs this) new-msg) (@(:matched-msgs this) new-msg))
          #{}
          (let [compat-msgs (filter #(compatible? % new-msg) @(:working-msgs this))
                merged-msgs (map #(merge-messages new-msg %) compat-msgs)
                new-merged-msgs (set (filter #(not (@(:working-msgs this) %)) merged-msgs))]
            (alter (:working-msgs this) union new-merged-msgs #{new-msg})
            (conj new-merged-msgs new-msg))))))
  (get-sent-messages
    [this chtype]
    (@(:sent-msgs this) chtype))
  (add-matched-and-sent-messages
    [this matched sent]
    (dosync
      (alter (:sent-msgs this) (partial merge-with union) sent)
      (alter (:matched-msgs this) union matched)
      (alter (:working-msgs this) difference matched))))

(defn make-linear-msg-set
  []
  (LinearMessageSet. (ref #{}) (ref #{}) (ref {})))