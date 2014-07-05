;;; CSNePS: Channel
;;; =============
;;; Daniel R. Schlegel
;;; Department of Computer Science and Engineering
;;; State University of New York at Buffalo
;;; drschleg@buffalo.edu

;;; The contents of this file are subject to the University at Buffalo
;;; Public License Version 1.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the
;;; License at http://www.cse.buffalo.edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;;; the License for the specific language governing rights and limitations
;;; under the License.
;;; 
;;; The Original Code is CSNePS.
;;; 
;;; The Initial Developer of the Original Code is Research Foundation of
;;; State University of New York, on behalf of University at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C) 2012
;;; Research Foundation of State University of New York, on behalf of
;;; University at Buffalo. All Rights Reserved.
;;; 
;;; Contributor(s): ______________________________________.

(in-ns 'csneps.core.build)

(declare valve-state-changed submit-to-channels new-message create-message-structure get-sent-messages)

(defn fix-fn-defs
  "A hack to work around circular reference issues. Otherwise we'd have to combine
   snip and build."
  [stc satc nm crs gsm bwi fwi]
  (def submit-to-channel stc)
  (def submit-assertion-to-channels satc)
  (def new-message nm)
  (def create-message-structure crs)
  (def get-sent-messages gsm)
  (def backward-infer bwi)
  (def forward-infer fwi))

(defrecord2 Channel 
  [originator    nil
   destination   nil
   waiting-msgs  (ref #{})   ;; Substitutions waiting at the valve.
   valve-open    (ref false) ;; A channels valve begins in the closed state. 
                             ;; It can be opened by the originator to invoke fwd inference, 
                             ;; or by the destination to invoke backward inference. 
   valve-selectors (ref #{}) ;; Valve selectors allow a message to pass a valve if it passes
                             ;; the condition of the selector. Each valve selector is a pair
                             ;; [subst context].
   filter-fn     nil
   switch-fn     nil
   switch-binds  nil
   filter-binds  nil])

(defmethod print-method csneps.core.build.Channel [o w]
  (.write ^java.io.Writer w 
    (str (print/term-printer (:originator o)) " -(" (count @(:waiting-msgs o)) ")"
         (cond
           (not @(:valve-open o)) "/"
           (seq @(:valve-selectors o)) "~"
           :else "-")
         "> " (print/term-printer (:destination o))
         " F: " (:filter-binds o)
         " S: " (:switch-binds o)
         "\n")))

;; Filter just makes sure that the incoming supstitutions is compatible. 

;; Switch applies incoming substitution to the terms of the switch (?) Maybe composition. 

;; Use agents for channels? Like message passing in Erlang?

(defn switch-fn
  [sub]
  (fn [varbinds]
    (if varbinds 
      (substitution-application sub varbinds)
      {})))

(defn filter-fn
  [subs]
  (fn [varbinds]
    (if varbinds
      (submap? subs varbinds)
      true)))

(defn find-channel 
  [originator destination]
  (some #(when (= (:name (:originator %)) (:name originator)) %) (@ant-in-channels destination)))

(defn install-channel
  [ch orig dest type]
  (dosync
    (condp = type
      :i-channel (if (set? (@i-channels orig))
                   (alter i-channels assoc orig (conj (@i-channels orig) ch))
                   (alter i-channels assoc orig (set (conj (@i-channels orig) ch))))
      :u-channel (if (set? (@u-channels orig))
                   (alter u-channels assoc orig (conj (@u-channels orig) ch))
                   (alter u-channels assoc orig (set (conj (@u-channels orig) ch))))
      :g-channel (if (set? (@g-channels orig))
                   (alter g-channels assoc orig (conj (@g-channels orig) ch))
                   (alter g-channels assoc orig (set (conj (@g-channels orig) ch)))))
    (if (set? (@ant-in-channels dest))
      (alter ant-in-channels assoc dest (conj (@ant-in-channels dest) ch))
      (alter ant-in-channels assoc dest (set (conj (@ant-in-channels dest) ch)))))
  ;; Focused forward-in-backward, extension for new in-channels.
  ;; We shouldn't continue inference backward if the orig was derived
  ;; because of the dest.
  (when (and (seq (@future-bw-infer dest))
             (@(:hyps (ct/currentContext)) orig))
    (backward-infer dest (@future-bw-infer dest) nil))
    
    ;; Send already produced msgs
  (when (@msgs orig)
    (doseq [msg (get-sent-messages (@msgs orig) type)]
      (submit-to-channel ch msg)))
  
  (when (and (seq (@future-fw-infer orig)) (ct/asserted? orig (ct/currentContext)))
    (forward-infer orig)))

(defn build-channel
  [originator destination target-binds source-binds]
  (let [target-binds (into {} (filter #(not (queryTerm? (second %))) target-binds))
        channel (or 
                  (find-channel originator destination)
                  (new-channel {:originator originator
                                :destination destination
                                :filter-fn (filter-fn target-binds)
                                :switch-fn (switch-fn source-binds)
                                :switch-binds source-binds
                                :filter-binds target-binds
                                :valve-open (ref false)}))]

    ;; The following section covers the following case: 
    ;; - originator is a term asserted at some earlier point.
    ;; - destination is currently being built.
    ;; Therfore, originator needs to send a message to destination
    ;; informing it that it is true.
    
    ;; Submit a message for the originator. 
    (when-not (variableTerm? originator)
      (submit-to-channel channel (new-message {:origin originator, :support-set #{['hyp #{(:name originator)}]}, :type 'I-INFER})))
    
    ;; When a term has a negation, submit a message saying so.
    (when-let [nor-cs (when (set? (@up-cablesetw originator))
                        ((@up-cablesetw originator) (slot/find-slot 'nor)))]
      (doseq [nor @nor-cs]
        (submit-to-channel channel (new-message {:origin originator, :support-set #{['der #{(:name nor)}]}, :type 'I-INFER, :true? false}))))
    channel))

(defn valve-open?
  [channel]
  @(:valve-open channel))

(defn pass-message?
  [channel message]
  (or
    (:fwd-infer? message)
    (valve-open? channel) ;; Kept for legacy reasons for now.
    (some #(and 
             ;; Empty map means pass everything in the ct. (Used with qvars)
             (or (= (first %) {}) (submap? (first %) (:subst message)))
             (some 
               (fn [supportset] 
                 (clojure.set/subset? (map get-term (second supportset)) @(:hyps (second %)))) 
               (:support-set message)))
          @(:valve-selectors channel))))

;; Watch the valve-state for changes. Adjust the operation of the channel as necessary.

(defn valve-state-changed
  [ref key oldvalue newvalue]
  (when newvalue
    ;; Handle opened valve
    nil))
