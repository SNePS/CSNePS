(ns csneps.snip.message
  (:use [clojure.set]
        [csneps.util])
  (:require [csneps.snip.originset :as os]
            [csneps.core :as csneps]
            [csneps.core.build :as build]))

;;; A message is a container for a substitution.

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
   antecedent-support-sets #{}
   type nil
   ;; In the case of a u-infer message, u-true? tells the receiver what it's 
   ;; new truth value is (i.e., whether to add the support-set to its OS, or
   ;; to build its negation and add it there).
   u-true? true 
   fwd-infer? false
   pos 0
   neg 0
   flaggedns {}
   invoke-set #{}
   taskid nil])

(defmethod print-method csneps.snip.message.Message [o w]
  (.write ^java.io.Writer w 
    (str "(" (:priority o) ")"
         " From: " (if (:origin o) (print-str (:origin o)) "<?>")
         " " (:type o) 
         (when (= (:type o) 'U-INFER ) (str " (" (if (:u-true? o) "t" "f") ")"))
         " pos:" (:pos o) " neg:" (:neg o)
         " support: " (:support-set o)
         " substitution: " (:subst o)
         " flaggedns: " (:flaggedns o)
         " forward?: " (:fwd-infer? o))))

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
                  :support-set (os/os-union (:support-set msg1) (:support-set msg2))
                  :antecedent-support-sets (union (:antecedent-support-sets msg1) (:antecedent-support-sets msg2)
                                                  (when-not (seq (:antecedent-support-sets msg1)) #{(:support-set msg1)})
                                                  (when-not (seq (:antecedent-support-sets msg2)) #{(:support-set msg2)}))
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
  [message & {:keys [origin priority subst support-set type u-true? fwd-infer? invoke-set taskid pos neg flaggedns]}]
  (let [new-u-true (cond 
                     (and (nil? type) (not= (:type message) 'U-INFER)) true
                     (and (not (nil? type)) (not= type 'U-INFER)) true ;; default to true in messages of the wrong type
                     (nil? u-true?) (:u-true? message)
                     :default u-true?)
        new-flaggedns (or flaggedns (:flaggedns message))
        new-pos (or pos (if (empty? new-flaggedns)
                          1
                          (count (filter true? (vals new-flaggedns)))))
        new-neg (max 0 (- (count new-flaggedns) new-pos))]
    ;; For some reason tests show using the -> with many inner assoc's is faster than 
    ;; one assoc with all of the k/v pairs. Made somewhat more efficient by using assoc-when-val
    ;; which will do the update only when the new val is truthy.
    (-> message 
      (assoc-when-val :origin origin)
      (assoc :priority (or priority (inc (:priority message))))
      (assoc-when-val :subst subst)
      (assoc-when-val :support-set support-set)
      (assoc :antecedent-support-sets #{})
      (assoc-when-val :type type)
      (assoc :u-true? new-u-true)
      (assoc-when-val :fwd-infer? fwd-infer?)
      (assoc :invoke-set (or invoke-set (if origin
                                          (@csneps/future-fw-infer origin)
                                          (when (:origin message) (@csneps/future-fw-infer (:origin message))))))
      (assoc-when-val :taskid taskid)
      (assoc :pos new-pos)
      (assoc :neg new-neg)
      (assoc :flaggedns new-flaggedns))))

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
  (-> message
    (assoc :priority 10)
    (assoc :type nil)
    (assoc :origin nil)
    (assoc :taskid nil)
    (assoc :invoke-set nil)
    (assoc :fwd-infer? false)))
