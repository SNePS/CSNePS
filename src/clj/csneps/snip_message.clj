(in-ns 'csneps.snip)

;;; A message is a container for a substitution. 

(defprotocol MessageStructure
  (get-rule-use-info [this new-msg]))

;; Type options:
;; Y-INFER
;; I-INFER
;; BACKWARD-INFER
;; CANCEL-INFER
;; UNASSERT

(defrecord2 Message
  [origin nil
   priority 1
   subst (ref {})
   support-set #{}
   type nil
   true? true
   fwd-infer? false
   pos 0
   neg 0
   flaggedns {}])

(defn message-key [msg]
  (set (keys @(:subst msg))))

(defn compatible? [msg1 msg2]
  "Returns true if the two Messages do not have contradictory 
   flagged node sets."
  (loop [fns1 (:flaggedns msg1)]
    (if (empty? fns1) 
      true
      (let [fn1p (second fns1)
            fn2p (get (:flaggedns msg2) (first fns1))]
        (if (or 
              (and (false? fn2p) (true? fn1p))
              (and (true? fn2p) (false? fn1p)))
          nil
          (recur (rest fns1)))))))

(defn merge-messages [msg1 msg2]
  (new-message {:subst (ref (merge @(:subst msg1) @(:subst msg2)))
                :pos (+ (:pos msg1) (:pos msg2))
                :neg (+ (:neg msg1) (:neg msg2))
                :support-set (union (:support-set msg1) (:support-set msg2))
                :flaggedns (clojure.core/merge (:flaggedns msg1) (:flaggedns msg2))
                :priority (max (:priority msg1) (:priority msg2))
                :fwd-infer? (or (:fwd-infer? msg1) (:fwd-infer? msg2))}))

(defn derivative-message 
  "Creates a message just like <message>, but with the given keys switched for the given values"
  [message & {:keys [origin substitution support-set type true? fwd-infer? pos neg flaggedns]}]
  (-> message 
    (assoc :origin (or origin (:origin message)))
    (assoc :priority (inc (:priority message)))
    (assoc :subst (or (ref substitution) (:subst message)))
    (assoc :support-set (or support-set (:support-set message)))
    (assoc :type (or type (:type message)))
    (assoc :true? (if (nil? true?) (:true? message) true?))
    (assoc :fwd-infer? (or fwd-infer? (:fwd-infer? message)))
    (assoc :pos (or pos (if (if (nil? true?) (:true? message) true?) 1 0)))
    (assoc :neg (or neg (if (if (nil? true?) (:true? message) true?) 0 1)))
    (assoc :flaggedns (or flaggedns 
                          {(or origin (:origin message)) (if (nil? true?) (:true? message) true?)}))))

(defn imessage-from-ymessage
  [message node]
  (derivative-message message 
                      :origin node
                      :support-set (conj (:support-set message) node)
                      :type 'I-INFER))
