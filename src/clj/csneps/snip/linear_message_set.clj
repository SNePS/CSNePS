(ns csneps.snip.linear_message_set
  (:use [csneps.snip.messagestructure]
        [csneps.snip.message]
        [clojure.set]))

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
  (seen-message?
    [this msg]
    (let [msg (sanitize-message msg)]
      (or (@(:working-msgs this) msg) (@(:matched-msgs this) msg))))
  (get-sent-messages
    [this chtype]
    (@(:sent-msgs this) chtype))
  (get-matched-messages
    [this]
    @(:matched-msgs this))
  (add-matched-and-sent-messages
    [this matched sent] (add-matched-and-sent-messages this matched sent true))
  (add-matched-and-sent-messages
    [this matched sent remove-matched-from-working?]
    (dosync
      (alter (:sent-msgs this) (partial merge-with union) sent)
      (alter (:matched-msgs this) union matched)
      (when remove-matched-from-working? (alter (:working-msgs this) difference matched))))
  (print-messages
    [this]
    (println "--- Linear Message Set ---")
    (println "Matched Messages:")
    (doseq [mm @(:matched-msgs this)] (println mm))
    (println "Sent Messages:")
    (doseq [[chtype sms] @(:sent-msgs this)
            sm sms] (println chtype ":" sm))
    (println "Working Messages:")
    (doseq [wm @(:working-msgs this)] (println wm))))

(defn make-linear-msg-set
  []
  (LinearMessageSet. (ref #{}) (ref #{}) (ref {})))