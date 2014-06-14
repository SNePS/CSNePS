(in-ns 'csneps.snip)

;;; A message is a container for a substitution. 

(defprotocol MessageStructure
  (get-rule-use-info [this new-msg]))

;; Type options:
;; U-INFER
;; I-INFER
;; G-INFER
;; BACKWARD-INFER
;; CANCEL-INFER

(defrecord2 Message
  [origin nil
   priority 1
   subst {}
   support-set #{}
   type nil
   true? true
   fwd-infer? false
   pos 0
   neg 0
   flaggedns {}
   invoke-set #{}
   taskid nil])

(defmethod print-method csneps.snip.Message [o w]
  (.write ^java.io.Writer w 
    (str "(" (:priority o) ")"
         " From: " (if (:origin o) (print-str (:origin o)) "<?>")
         " " (:type o) " (" (if (:true? o) "t" "f") ")"
         " pos:" (:pos o) " neg:" (:neg o)
         " support: " (:support-set o)
         " substitution: " (:subst o))))

(defn message-key [msg]
  (set (keys (:subst msg))))

(defn compatible? [msg1 msg2]
  "Returns true if the two Messages do not have contradictory 
   flagged node sets, and their substitutions are compatible."
  (and 
    (build/compatible-substitutions? (:subst msg1) (:subst msg2))
    (loop [fns1 (:flaggedns msg1)]
      (if (empty? fns1) 
        true
        (let [fn1p (second fns1)
              fn2p (get (:flaggedns msg2) (first fns1))]
          (when-not (or 
                      (and (false? fn2p) (true? fn1p))
                      (and (true? fn2p) (false? fn1p)))
            (recur (rest fns1))))))))

(defn merge-two-messages
  [msg1 msg2]
  (let [new-flaggedns (clojure.core/merge (:flaggedns msg1) (:flaggedns msg2))]
    (new-message {:subst (merge (:subst msg1) (:subst msg2))
                  :pos (count (filter true? (vals new-flaggedns)))
                  :neg (count (filter false? (vals new-flaggedns)))
                  :support-set (os-union (:support-set msg1) (:support-set msg2))
                  :flaggedns new-flaggedns
                  :priority (max (:priority msg1) (:priority msg2))
                  :fwd-infer? (or (:fwd-infer? msg1) (:fwd-infer? msg2))
                  :invoke-set (union (:invoke-set msg1) (:invoke-set msg2))})))

(defn merge-messages 
  [& msgs]
  (cond
    (empty? msgs)
    nil
    (= (count msgs) 1)
    (first msgs)
    :else
    (reduce merge-two-messages msgs)))

(defn derivative-message 
  "Creates a message just like <message>, but with the given keys switched for the given values"
  [message & {:keys [origin priority subst support-set type true? fwd-infer? invoke-set taskid pos neg flaggedns]}]
  (-> message 
    (assoc :origin (or origin (:origin message)))
    (assoc :priority (or priority (inc (:priority message))))
    (assoc :subst (or subst (:subst message)))
    (assoc :support-set (or support-set (:support-set message)))
    (assoc :type (or type (:type message)))
    (assoc :true? (if (nil? true?) (:true? message) true?))
    (assoc :fwd-infer? (or fwd-infer? (:fwd-infer? message)))
    (assoc :invoke-set (or invoke-set (if origin
                                        @(:future-fw-infer origin)
                                        (when (:origin message) @(:future-fw-infer (:origin message))))))
    (assoc :taskid (or taskid (:taskid message)))
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

(defn sanitize-message
  "Produces a message without the fields which vary without
   actually indicating the messages represent different information."
  [message]
  (derivative-message message
                      :priority 10
                      :fwd-infer? false))
