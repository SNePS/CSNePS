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

(declare valve-state-changed submit-to-channels new-message create-message-structure)

(defn fix-fn-defs
  "A hack to work around circular reference issues. Otherwise we'd have to combine
   snip and build."
  [stc satc nm crs bwi fwi]
  (def submit-to-channel stc)
  (def submit-assertion-to-channels satc)
  (def new-message nm)
  (def create-message-structure crs)
  (def backward-infer bwi)
  (def forward-infer fwi))

(defrecord2 Channel 
  [originator    nil
   destination   nil
   waiting-msgs  (ref #{})   ;; Substitutions waiting at the valve.
   valve-open    (ref false) ;; A channels valve begins in the closed state. 
                             ;; It can be opened by the originator to invoke fwd inference, 
                             ;; or by the destination to invoke backward inference. 
   filter-fn     nil
   switch-fn     nil])

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
      (subset? subs varbinds)
      true)))

(defn find-channel 
  [originator destination]
  (some #(when (= (:originator %) originator) %) @(:ant-in-channels destination)))

(defn install-channel
  [ch orig dest type]
  (dosync
    (condp = type
      :i-channel (alter (:i-channels orig) conj ch)
      :u-channel (alter (:u-channels orig) conj ch)
      :g-channel (alter (:g-channels orig) conj ch))
    (alter (:ant-in-channels dest) conj ch))
  ;; Focused forward-in-backward, extension for new in-channels.
  (when (seq @(:future-bw-infer dest))
    (backward-infer dest @(:future-bw-infer dest) nil))
  (when (seq @(:future-fw-infer orig))
    (forward-infer orig)))

(defn build-channel
  [originator destination target-binds source-binds]
  (let [channel (or 
                  (find-channel originator destination)
                  (new-channel {:originator originator
                                :destination destination
                                :filter-fn (filter-fn target-binds)
                                :switch-fn (switch-fn source-binds)
                                :valve-open (ref false)}))]

    ;; The following section covers the following case: 
    ;; - originator is a term asserted at some earlier point.
    ;; - destination is currently being built.
    ;; Therfore, originator needs to send a message to destination
    ;; informing it that it is true.
    
    ;; Inform the I-Channels that this is true.
    (when (ct/asserted? originator (ct/currentContext))
      (submit-to-channel channel (new-message {:origin originator, :support-set #{originator}, :type 'I-INFER})))
    ;; Handle negations
    (let [nor-cs (@(:up-cablesetw originator) (slot/find-slot 'nor))
          up-term (when nor-cs (some #(when (ct/asserted? % (ct/currentContext)) %) @nor-cs))]
      (when up-term
        (submit-to-channel channel (new-message {:origin originator, :support-set #{up-term}, :type 'I-INFER, :true? false}))))
    channel))


(defn valve-open?
  [channel]
  @(:valve-open channel))


;; Watch the valve-state for changes. Adjust the operation of the channel as necessary.

(defn valve-state-changed
  [ref key oldvalue newvalue]
  (when newvalue
    ;; Handle opened valve
    nil))
