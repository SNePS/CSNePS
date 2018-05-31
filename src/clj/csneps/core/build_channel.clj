;;; CSNePS: Channel
;;; =============
;;; Daniel R. Schlegel
;;; Department of Computer Science
;;; State University of New York at Oswego
;;; daniel.schlegel@oswego.edu

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
;;; Portions created by the Initial Developer are Copyright (C) 2016
;;; Research Foundation of State University of New York, on behalf of
;;; University at Buffalo. All Rights Reserved.
;;; 
;;; Contributor(s): ______________________________________.

(in-ns 'csneps.core.build)

(declare valve-state-changed submit-to-channels new-message create-message-structure get-sent-messages add-matched-and-sent-messages)

(defn fix-fn-defs
  "A hack to work around circular reference issues. Otherwise we'd have to combine
   snip and build."
  [stc bstc satc nm crs gsm bwi fwi amasm]
  (def submit-to-channel stc)
  (def blocking-submit-to-channel bstc)
  (def submit-assertion-to-channels satc)
  (def new-message nm)
  (def create-message-structure crs)
  (def get-sent-messages gsm)
  (def backward-infer bwi)
  (def forward-infer fwi)
  (def add-matched-and-sent-messages amasm))

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
           (seq @(:valve-selectors o)) 
           (let [selectors-this-ct (filter #(clojure.set/subset? @(:hyps (ct/find-context (second %))) @(:hyps (ct/currentContext))) @(:valve-selectors o))]
             (str "~" (print-str (map #(first %) selectors-this-ct)) "~"))
           (not @(:valve-open o)) "/"
           :else "-")
         "> " (print/term-printer (:destination o))
         " F: " (:filter-binds o)
         " S: " (:switch-binds o)
         "\n")))

;; Filter just makes sure that the incoming supstitutions is compatible. 

;; Switch applies incoming substitution to the terms of the switch (?) Maybe composition. 

;; Use agents for channels? Like message passing in Erlang?

(defn clean-subst
  "Removes any irrelevant substitutions (those not in the destination of the channel) from the passing message."
  [subst ch]
  (let [vars (set (filter variable? (flatten-term (:destination ch))))]
    (into {} (filter #(get vars (first %)) subst))))

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
  (let [inch (@ant-in-channels destination)
        out-ich (@i-channels originator)
        out-uch (@u-channels originator)
        out-gch (@g-channels originator)]
    (if (< (count inch) (+ (count out-ich) (count out-uch) (count out-gch)))
      (some #(when (= (:name (:originator %)) (:name originator)) %) inch)
      (letfn [(nameeq [ch] 
                (when (= (:name (:destination ch)) 
                         (:name destination)) 
                  ch))]
        (or 
          (some nameeq out-ich)
          (some nameeq out-gch)
          (some nameeq out-uch))))))

(defn install-channel
  ""
  [ch orig dest type]
  (when-not (find-channel orig dest)
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
  
    ;(println "installing channel:" ch (when (@msgs orig) (get-sent-messages (@msgs orig) type)))
  
  
    ;; The following section covers the following case: 
    ;; - originator is a term asserted at some earlier point.
    ;; - destination is currently being built.
    ;; Therfore, originator needs to send a message to destination
    ;; informing it that it is true.
  
    ;; Submit a message for the originator. 
    (when (and 
            (or (= type :i-channel)
                (= type :g-channel))
            (not (variableTerm? orig))
            (not= (syntactic-type-of orig) :csneps.core/Negation))
      (submit-to-channel ch (new-message {:origin orig, :support-set #{['hyp #{(:name orig)}]}, :type 'I-INFER})))
    
    ;; Remmember, inner terms are built before outer terms, so to handle negations, they must come when the nor is
    ;; built. If orig is a nor, send a u-infer message to its arguments.
    (when (and (= type :u-channel)
               (= (syntactic-type-of orig) :csneps.core/Negation))
      (let [new-msg (new-message {:origin orig, :support-set #{['der #{(:name orig)}]}, :type 'U-INFER, :true? false})]
        (submit-to-channel ch new-msg)))

  ;; Focused forward-in-backward, extension for new in-channels.
  ;; TODO: We shouldn't continue inference backward if the orig was derived
  ;; because of the dest.
    (when (and (seq @(:future-bw-infer dest)))
    ;             (ct/asserted? orig (ct/currentContext))) ;; This doesn't work - assert hasn't been done yet when chs are built.
      (backward-infer dest @(:future-bw-infer dest) nil))
    
    (when (@msgs orig)
      (doseq [msg (get-sent-messages (@msgs orig) type)]
        ;; We MUST block until the message is processed, otherwise the following can happen:
        ;; 1) This inference sends a new message to out going channels (of which there are none) and saves it
        ;; 2) At the same time, a new channel is built, seeing no messages to send.
        ;; So the new message is never sent on the new channel.
        (submit-to-channel ch msg)))))

(defn build-channel
  [originator destination target-binds source-binds]
  (let [qvar-targets (into {} (filter #(queryTerm? (second %)) target-binds))
        source-binds (merge source-binds (set/map-invert qvar-targets))
        target-binds (apply dissoc target-binds (keys qvar-targets))
        channel (or 
                  (find-channel originator destination)
                  (new-channel {:originator originator
                                :destination destination
                                :filter-fn (filter-fn target-binds)
                                :switch-fn (switch-fn source-binds)
                                :switch-binds source-binds
                                :filter-binds target-binds
                                :valve-open (ref false)}))]
    channel))

(defn valve-open?
  [channel]
  @(:valve-open channel))

(defn msg-semtype-check
  [msg] 
  ;(when-not (every? (fn [[v1 v2]] (subtypep (semantic-type-of v2) (semantic-type-of v1))) (:subst msg))
  ;   (send screenprinter (fn [_]  (println "Semtype check failed: " (:subst msg)))))
  (every? (fn [[v1 v2]] (subtypep (semantic-type-of v2) (semantic-type-of v1))) (:subst msg)))

(defn pass-vs?
  [[sub ct] message]
  (let [ct (ct/find-context ct)]
    (and 
      ;; Empty map means pass everything in the ct. (Used with qvars)
      (or (= sub {}) (submap? sub (:subst message)))
      (or (empty? (:support-set message)) ;; Empty support set means we believe it based on nothing at all.
          (some 
            (fn [supportset] 
              (clojure.set/subset? (second supportset) (ct/hyps ct))) 
            (:support-set message))))))

(defn pass-message?
  [channel message]
  (and 
    ;; This will prevent passing some messages which may eventually have reason to pass
    ;; but don't currently. Remember that semtypes can get lowered, so just because a term 
    ;; is, e.g., an Entity right now therefore can't match with (every x Thing), it may
    ;; eventually become a Thing based on usage. (See unification/match-single-unifier for
    ;; more explaination)
    ;; Removed on 5/31/18, since currently there's no mechanism to re-try a message which has
    ;; failed for this reason. It would require additional hooks into the semtype lowering code.
    ;;(msg-semtype-check message)
	  (or
	    (:fwd-infer? message)
	    (valve-open? channel) ;; Kept for legacy reasons for now.
	    (some #(pass-vs? % message) @(:valve-selectors channel)))))

;; Watch the valve-state for changes. Adjust the operation of the channel as necessary.

(defn valve-state-changed
  [ref key oldvalue newvalue]
  (when newvalue
    ;; Handle opened valve
    nil))

;;; Debug functions:

(defn print-channel-and-msgs
  [from to]
  (let [from (get-term from)
        to (get-term to)
        chs (concat (@i-channels from) (@u-channels from) (@g-channels from))
        ch (first (filter #(= (:destination %) to) chs))]
    (println "Channel" ch)
    (println @(:waiting-msgs ch))))
    
  
  
  
