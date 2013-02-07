(in-ns 'csneps.snip)

;;; A message is a container for a substitution. 

;; Type options:
;; Y-INFER
;; I-INFER
;; BACKWARD-INFER
;; CANCEL-INFER
;; UNASSERT

(defrecord2 Message
  [origin nil
   priority 1
   substitution {}
   support-set #{}
   type nil
   true? true
   fwd-infer? false])

(defn RUI->message
  ([rui node type] (RUI->message rui node type true true))
  ([rui node type true?] (RUI->message rui node type true? true))
  ([rui node type true? fwd-infer?]
;    (println node (clojure.core/type node) 
;             (:support-set 
;                              (->Message node 1 {} (conj (:support-set rui) node) type true? fwd-infer?)
;                              
;                              
;                              )
;             (let [a (second (:support-set 
;                              (->Message node 1 {} (conj (:support-set rui) node) type true? fwd-infer?)
;                              
;                              
;                              ))]
;               (println a)
;               (str a "-"  (type a))))
    (->Message node 1 (:subs rui) (conj (:support-set rui) node) type true? fwd-infer?)))
;    (new-message {:origin node
;                  :substitution (:subs rui)
;                  :support-set (conj (:support-set rui) node)
;                  :type type
;                  :true? true?
;                  :fwd-infer? fwd-infer?})))

(defn derivative-message 
  "Creates a message just like <message>, but with the given keys switched for the given values"
  [message & {:keys [origin substitution support-set type true? fwd-infer?]}]
  (new-message {:origin (or origin (:origin message))
                :priority (inc (:priority message))
                :substitution (or substitution (:substitution message))
                :support-set (or support-set (:support-set message))
                :type (or type (:type message))
                :true? (if (nil? true?) (:true? message) true?)
                :fwd-infer? (or fwd-infer? (:fwd-infer? message))}))

(defn imessage-from-ymessage
  [message node]
  (derivative-message message 
                      :origin node
                      :support-set (conj (:support-set message) node)
                      :type 'I-INFER))

(defn derive-forward-infer
  [message node]
  (derivative-message message 
                      :origin node
                      :support-set (conj (:support-set message) node)))
