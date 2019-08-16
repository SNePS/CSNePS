;; This message set is meant to handle the case where the down cableset only contains one item, so any relevant
;; message which is not a duplicate should just be returned. It does not track working messages, as there should be none.
;; It's somewhat debatable whether this is really needed as a separate structure or if linear / ptree can be adapted to
;; handle this case.

(ns csneps.snip.passthrough-message-set
  (:use [csneps.snip.messagestructure]
        [csneps.snip.message]
        [clojure.set]))

(defrecord PassthroughMessageSet
  [matched-msgs
   working-msgs
   sent-msgs]
  MessageStructure
  (get-new-messages [this new-msg]
    ;; if new-msg isn't actually new, return empty set.
    (let [new-msg (sanitize-message new-msg)]
      (if (@(:matched-msgs this) new-msg)
        #{}
        #{new-msg})))
  (seen-message?
    [this msg]
    (@(:matched-msgs this) (sanitize-message msg)))
  (get-sent-messages
    [this chtype]
    (@(:sent-msgs this) chtype))
  (get-matched-messages
    [this]
    @(:matched-msgs this))
  (add-matched-and-sent-messages
    [this matched sent] (add-matched-and-sent-messages this matched sent true))
  (add-matched-and-sent-messages
    [this matched sent _]
    (dosync
      (alter (:sent-msgs this) (partial merge-with union) sent)
      (alter (:matched-msgs this) union matched)))
  (print-messages
    [this]
    (println "--- Passthrough Message Set ---")
    (println "Matched Messages:")
    (doseq [mm @(:matched-msgs this)] (println mm))
    (println "Sent Messages:")
    (doseq [[chtype sms] @(:sent-msgs this)
            sm sms] (println chtype ":" sm))))

(defn make-passthrough-msg-set
  []
  (PassthroughMessageSet. (ref #{}) (ref #{}) (ref {})))