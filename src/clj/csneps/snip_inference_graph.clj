(in-ns 'csneps.snip)

(load "snip_beliefrevision")
(load "snip_message")
(load "snip_linear_msg_set")
(load "snip_sindex")
(load "snip_ptree")

;; Tracking inference tasks
(def ^:dynamic taskid 0)

(declare initiate-node-task create-message-structure get-new-messages open-valve cancel-infer-of)

;;;;;;;;;;;;;
;;; Debug ;;;
;;;;;;;;;;;;;

(def print-intermediate-results false)
(def print-results-on-infer false)
(def showproofs true)

(defn ig-debug-all []
  (set-debug-features :msgtx, :msgrx, :valveselect, :bw, :cancel, :switch, :filter, :rui, :der, :newmsg, :os, :infer)
  (set-debug-nodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Concurrency control ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Incremented whenever a message is submitted, and decremented once inference
;; on a message is complete, this allows us to determine when the graph is in
;; a quiescent state.
(def infer-status (ref {nil (edu.buffalo.csneps.util.CountingLatch.)}))

;; Tasks have :priority metadata. This allows the queue to order them
;; properly for execution. Higher priority is executed first.
(def task-cmpr (proxy [Comparator] []
                 (compare [a b]
                          (let [p_a (:priority (meta a))
                                p_b (:priority (meta b))]
                            (cond
                              (= p_a p_b) 0
                              (> p_a p_b) -1
                              :else 1)))))

;; Priority Blocking Queue to handle the tasks.
(def queue (PriorityBlockingQueue. 50 task-cmpr))

;(def queue (edu.buffalo.csneps.util.BlockingLifoQueue.))

;(def queue (LinkedBlockingQueue.))

;; Fixed Thread Pool of size 2 * processors, using queue as it's queue.
(def executorService (ThreadPoolExecutor.
                       ig-cpus-to-use
                       ig-cpus-to-use
                       (Long/MAX_VALUE) TimeUnit/NANOSECONDS queue))
(.prestartAllCoreThreads ^ThreadPoolExecutor executorService)

(defn resetExecutor
  []
  (.clear ^PriorityBlockingQueue queue)
  (.shutdownNow ^ThreadPoolExecutor executorService)
  (def executorService (ThreadPoolExecutor.
                         ig-cpus-to-use
                         ig-cpus-to-use
                         (Long/MAX_VALUE) TimeUnit/NANOSECONDS queue))
  (.prestartAllCoreThreads ^ThreadPoolExecutor executorService)
  (def infer-status (ref {nil (edu.buffalo.csneps.util.CountingLatch.)})))

;; Only used when exiting. 
(defn shutdownExecutor
  []
  (.shutdownNow ^ThreadPoolExecutor executorService))

;;; Experimental attempt at pausing inference.
(let [waiting-queue (LinkedBlockingQueue.)]
  (defn pause-execute 
    []
    (.drainTo ^PriorityBlockingQueue queue waiting-queue))
  
  (defn resume-execute
    []
    (.drainTo ^LinkedBlockingQueue waiting-queue queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel Maniupulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn priority-partial
  [priority f & more] 
  (with-meta (apply partial f more) {:priority priority}))

(defn submit-to-channel
  [^csneps.core.build.Channel channel ^csneps.snip.Message message]
  (print-debug :msgtx #{(:originator channel) (:destination channel)} (print-str "MSGTX: " message "\n -- on channel" channel))
  ;; Filter
  (print-debug :filter #{(:originator channel) (:destination channel)} (print-str "FILTER: " ((:filter-fn channel) (:subst message)) "\n -- in channel" channel))
  (when ((:filter-fn channel) (:subst message))
    ;; Switch
    (print-debug :switch #{(:originator channel) (:destination channel)} (print-str "SWITCH: " ((:switch-fn channel) (:subst message)) "\n -- in channel" channel))
    (let [message (derivative-message message :subst ((:switch-fn channel) (:subst message)))]
      ;; Previously the above cleaned the unused substitutions. I think this is a bad idea, since in cases
      ;; of embedded arbs, the "extra" substitutions matter for determining a match!
      ;; Old code did: (build/clean-subst ((:switch-fn channel) (:subst message)) channel))]
      (print-debug :valveselect #{(:originator channel) (:destination channel)} (print-str "VS: " (build/pass-message? channel message) "\n -- for message" message" \n   -- in channel" channel))
      (if (build/pass-message? channel message)
        ;; Process the message immediately. For forward infer, this ammounts to 
        ;; ignoring the status of the valve.
        (do 
          (when (@infer-status (:taskid message))
            (.increment ^CountingLatch (@infer-status (:taskid message))))
          (print-debug :msgrx #{(:originator channel) (:destination channel)} (print-str "MSGRX: " message "\n -- on channel" channel))
          (.execute ^ThreadPoolExecutor executorService 
            (priority-partial 1 initiate-node-task (:destination channel) message)))
        ;; Cache in the waiting-msgs
        (dosync (alter (:waiting-msgs channel) conj message))))))

(defn blocking-submit-to-channel
  [ch msg]
  (let [taskid (gensym "task")]
    (dosync (alter infer-status assoc taskid (edu.buffalo.csneps.util.CountingLatch.)))
    (submit-to-channel ch (assoc msg :taskid taskid))
    (.await ^CountingLatch (@infer-status taskid))))

(defn submit-assertion-to-channels
  [term & {:keys [fwd-infer] :or {fwd-infer false}}]
  ;; Send the information about this assertion forward in the graph.
  ;; Positive instances:
  (let [msg (new-message {:origin term, 
                          :support-set #{['hyp #{(:name term)}]}, 
                          :type 'I-INFER, 
                          :fwd-infer? fwd-infer
                          :pos 1
                          :neg 0
                          :flaggedns {term true}})]
    (doall (map #(submit-to-channel % msg) (@i-channels term))))
  ;; Conjunctions:
  ;; Conjunctions have no antecedents if they are true, so the U-INFER messages must be passed on here
  (when (= (type-of term) :csneps.core/Conjunction)
    (let [msg (new-message {:origin term, :support-set #{['der #{(:name term)}]}, :type 'U-INFER, :fwd-infer? fwd-infer})]
      (doall (map #(submit-to-channel % msg) (@u-channels term))))))

;(defn open-valve 
;  [channel taskid]
;  ;; Start by opening the channel. Anything new should go right to the executor pool.
;  (dosync (ref-set (:valve-open channel) true))
;  ;; Add the waiting messages to the executor pool.
;  (doseq [msg @(:waiting-msgs channel)]
;    ;(send screenprinter (fn [_]  (println "inc-ov" taskid)))
;    (when taskid (.increment (@infer-status taskid)))
;    (.execute ^ThreadPoolExecutor executorService (priority-partial 1 initiate-node-task (:destination channel) (derivative-message msg :taskid taskid))))
;  ;; Clear the waiting messages list.
;  (dosync (alter (:waiting-msgs channel) empty)))
;
;(defn close-valve
;  [channel]
;  (dosync (ref-set (:valve-open channel) false)))

(defn add-valve-selector
  [channel subst context taskid]
  (let [vars-in-orig (build/get-vars (:originator channel))
        ;; selector subst is the same, doesn't have the filtered parts, since those have already
        ;; been checked!
        selector-subst (build/substitution-application-nomerge subst
                                                               (or (:switch-binds channel) #{}))
        selector-subst (into {} (filter #(vars-in-orig (first %)) subst))
        ;; subst is the subst to pass onward. (needs filtered parts too)
        subst (build/substitution-application-nomerge (merge subst (:filter-binds channel))
                                                      (or (:switch-binds channel) #{}))
        subst (into {} (filter #(vars-in-orig (first %)) subst))
        valve-selector [selector-subst (:name context)]]
    ;; Only add the selector and check for new matching messages if this is actually a new selector.
    ;; New messages will otherwise be checked upon being submitted to the channel.
    (if-not (@(:valve-selectors channel) valve-selector)
      (do 
        (dosync (commute (:valve-selectors channel) conj valve-selector))
        (doseq [msg @(:waiting-msgs channel)]
          ;(send screenprinter (fn [_] (println "Trying to pass" msg)))
          (print-debug :valveselect #{(:originator channel) (:destination channel)} (print-str "VS (new): " (build/pass-vs? valve-selector msg) "\n -- for message" msg " \n   -- in channel" channel))
          (when (build/pass-vs? valve-selector msg)
            (dosync (commute (:waiting-msgs channel) disj msg))
            (if (@infer-status taskid)
              (do 
                ;(send screenprinter (fn [_]  (println "inc-stc" taskid (derivative-message msg :taskid taskid))))
                (.increment ^CountingLatch (@infer-status taskid)))
              (send screenprinter (fn [_]  
                                     (println) 
                                     (println "Warning: No taskid when adding valve selector to" channel) 
                                     (println "This may indicate a bug or race condition in the inference graph.")
                                     (println))))
            (print-debug :msgrx #{(:originator channel) (:destination channel)} (print-str "MSGRX: " msg "\n -- on channel" channel))
            (.execute ^ThreadPoolExecutor executorService 
              (priority-partial 1 initiate-node-task (:destination channel) (derivative-message msg :taskid taskid)))))
        subst)
      nil)))

(def hyp-subst-of-ct?
  (memo
    (fn [hyps ct]
      (clojure.set/subset? 
        hyps
        @(:hyps ct)))))

;;; Two different sets of args: 
;;; 1) [channel context] to be called by introduction rules, since once a rule has been
;;; introduced in a context, it doesn't need to be again.
;;; 2) [channel subst context] to be called for halting the derivation of specific
;;; instances, since the subst matters.
(defn remove-valve-selector
  ([channel hyps] (remove-valve-selector channel nil hyps))
  ([channel subst hyps]
    (let [vars-in-orig (when subst (build/get-vars (:originator channel)))
          selector-subst (build/substitution-application-nomerge subst
                                                                 (or (:switch-binds channel) #{}))
          selector-subst (into {} (filter #(vars-in-orig (first %)) subst))
          ;; subst is the subst to pass onward. (needs filtered parts too)
          subst (build/substitution-application-nomerge (merge subst (:filter-binds channel))
                                                        (or (:switch-binds channel) #{}))
          subst (into {} (filter #(vars-in-orig (first %)) subst))
          ;is-rel-vs? (fn [vs] (some #(clojure.set/subset? 
          ;                             (second %) 
          ;                             (set (map (fn [h] (:name h)) @(:hyps (second vs))))) 
          ;                          hyps))
          is-rel-vs? (fn [vs] (let [ct (ct/find-context (second vs))]
                                (some #(hyp-subst-of-ct? (second %) ct) hyps)))
          rel-vses (filter is-rel-vs? @(:valve-selectors channel))
          match-vses (when selector-subst (filter #(submap? selector-subst (first %)) rel-vses))]
      ;(if (seq rel-vses)
      ;(send screenprinter (fn [_] (println channel vars-in-orig subst "VSes" (map first @(:valve-selectors channel)))));)
      
      (if selector-subst
        (dosync (alter (:valve-selectors channel) difference match-vses))
        (dosync (alter (:valve-selectors channel) difference rel-vses)))
      ;(if subst
      ;  (when (seq match-vses) true)
      ;  (when (seq rel-vses) true))
      (when (seq match-vses) subst))))

;;;;;;;;;;;;;;;;;
;;; Inference ;;;
;;;;;;;;;;;;;;;;;

(defn negated?
  ([term]
    (negated? term (ct/currentContext)))
  ([term context]
    (let [negation (get-froms #{term} (slot/find-slot 'nor))]
      (when-not (empty? negation)
        (ct/asserted? (first negation) context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn backward-infer
  "Spawns tasks recursively to open valves in channels and begin performing
   backward-chaining inference. The network tries to derive term, and uses a
   set to track which nodes it has already visited."
  ([term] 
    (let [taskid (gensym "task")] 
      (dosync (commute infer-status assoc taskid (edu.buffalo.csneps.util.CountingLatch.)))
      (backward-infer term -10 #{} #{term} {} (ct/currentContext) taskid)))
  ([term taskid] 
    (let [gt (gensym "task")
          taskid (if taskid 
                   taskid
                   gt)]
      (dosync (commute infer-status assoc taskid (edu.buffalo.csneps.util.CountingLatch.)))
      (backward-infer term -10 #{} #{term} {} (ct/currentContext) taskid)))
  ([term invoketermset taskid] 
    (let [gt (gensym "task")
          taskid (if taskid 
                   taskid
                   (do 
                     (dosync (commute infer-status assoc gt (edu.buffalo.csneps.util.CountingLatch.)))
                     gt))]
    (backward-infer term -10 #{} invoketermset {} (ct/currentContext) taskid)))
  ;; Opens appropriate in-channels, sends messages to their originators.
  ([term depth visited invoketermset subst context taskid] 
    
    (dosync (commute (:future-bw-infer term) union invoketermset))
    
    ;; Some rules need special backward-inferring into. 
    (when-not (ct/asserted? term context)
      (cond 
        (and 
          (= (syntactic-type-of term) :csneps.core/Implication)
          (= (count (first (@down-cableset term))) 1))
        (let [ant (ffirst (@down-cableset term))
              ct (if ((ct/hyps (ct/currentContext)) ant)
                   (ct/currentContext)
                   (ct/defineContext 
                     (gensym (:name (ct/currentContext))) 
                     :parents (list (ct/currentContext))
                     :hyps #{(:name ant)}))]
          (doseq [cq (second (@down-cableset term))]    
            (when taskid (.increment ^CountingLatch (@infer-status taskid)))
            (.execute ^ThreadPoolExecutor executorService 
              (priority-partial depth 
                                (fn [t d v i s c id] 
                                  (backward-infer t d v i s c id) 
                                  (when id (.decrement ^CountingLatch (@infer-status id))))
                                cq
                                (dec depth)
                                (conj visited term)
                                invoketermset
                                subst
                                ct
                                taskid))))))
    
    ;; Propogate backward-infer messages.
    (doseq [ch (union (@ant-in-channels term) (@semtype-in-channels term) {})]
      (when (and (not (visited term)))
;                 (if-let [oppositech (build/find-channel (:destination ch) (:originator ch))]
;                   (not (visited (:originator ch)))
;                   true))
                 
                 ;(not (visited (:originator ch))))
                 ;(not (subset? invoketermset @(:future-bw-infer (:originator ch)))))
                 ;(not= (union @(:future-bw-infer (:originator ch)) invoketermset) @(:future-bw-infer (:originator ch))))
        (print-debug :bw #{(:originator ch) (:destination ch)} (print-str "BW: Backward Infer -" depth "- opening channel from" (:originator ch) "to" term "(task" taskid")"))
        (let [subst (add-valve-selector ch subst context taskid)]
          ;; Semtype channels won't have an originator, so no need to propogate backward.
          (when (:originator ch)
	          (when subst ;; Will be nil if this is an old VS.
	            (if (and taskid
	                     (@infer-status taskid))
	              (when taskid (.increment ^CountingLatch (@infer-status taskid)))
	              (send screenprinter (fn [_]  
	                                    (println) 
	                                    (println "Warning: No taskid during backward infer on" ch) 
	                                    (println "This may indicate a bug or race condition in the inference graph.")
	                                    (println))))
              (.execute ^ThreadPoolExecutor executorService 
                    (priority-partial depth 
                                      (fn [t d v i s c id] 
                                        (backward-infer t d v i s c id) 
                                        ;(send screenprinter (fn [_]  (println "dec-bwi" id))) 
                                        (when (@infer-status id) (.decrement ^CountingLatch (@infer-status id))))
                                      (:originator ch)
                                      (dec depth)
                                      (conj visited term)
                                      invoketermset
                                      subst
                                      context
                                      taskid)))))))))

(defn cancel-infer
  "Same idea as backward-infer, except it closes valves. Cancelling inference
   has top priority."
  ([term] (cancel-infer term nil nil {} (@support term)))
  ([term cancel-for-term] (cancel-infer term cancel-for-term nil {} (@support term)))
  ([term cancel-for-term taskid] (cancel-infer term cancel-for-term taskid {} (@support term))) 
  ([term cancel-for-term taskid subst hyps]
    (when cancel-for-term
      (dosync (commute (:future-bw-infer term) disj cancel-for-term)))
    ;(when (empty? @(:future-bw-infer term))
      (let [affected-chs (doall 
                           (for [ch (union (@ant-in-channels term) (@semtype-in-channels term))
                                 :let [new-subst (remove-valve-selector ch subst hyps)]
                                 :when (not (nil? new-subst))]
                             [ch new-subst]))]
        (when (seq affected-chs)
          (print-debug :cancel #{term} (print-str "CANCEL: Cancel Infer - closing incoming channels to" term))
          (doseq [[ch subst] affected-chs]
            (when (:originator ch)
	            (when taskid (.increment ^CountingLatch (@infer-status taskid)))
	            (.execute ^ThreadPoolExecutor executorService 
	                (priority-partial Integer/MAX_VALUE 
	                                  (fn [t c id s h] (cancel-infer t c id s h) (when id (.decrement ^CountingLatch (@infer-status id))))
	                                  (:originator ch) 
	                                  cancel-for-term 
	                                  taskid
	                                  subst
	                                  hyps))))))))

(defn forward-infer
  "Begins inference in term. Ignores the state of valves in sending I-INFER and U-INFER messages
   through the graph."
  [term]
  (let [taskid (gensym "task")]
    (dosync (alter infer-status assoc taskid (edu.buffalo.csneps.util.CountingLatch.)))
    ;; We need to pretend that a U-INFER message came in to this node.
    ;(send screenprinter (fn [_]  (println "inc-fwi")))
    (.increment ^CountingLatch (@infer-status taskid))
    (.execute ^ThreadPoolExecutor executorService 
      (priority-partial 1 initiate-node-task term 
                        (new-message {:origin nil, :support-set #{}, :type 'U-INFER, :fwd-infer? true :invoke-set #{term} :taskid taskid})))
    (.await ^CountingLatch (@infer-status taskid))))

(defn cancel-forward-infer
  ([term] (cancel-infer term term))
  ([term cancel-for-term]
    (when cancel-for-term
      (dosync (alter future-fw-infer assoc term (disj (@future-fw-infer term) cancel-for-term))))
    (doseq [ch (concat (@i-channels term) (@u-channels term) (@g-channels term))]
      (.increment ^CountingLatch (@infer-status nil))
      (.execute ^ThreadPoolExecutor executorService 
        (priority-partial Integer/MAX_VALUE 
                          (fn [t c] (cancel-forward-infer t c) (.decrement ^CountingLatch (@infer-status nil)))
                          (:destination ch) cancel-for-term)))))

;; How do we ensure no re-derivations?
;; When do we call this? 
(defn derive-otherwise 
  "Attempt derivation using methods other than the IG"
  [p]
  (setOr
    (sort-based-derivable p (ct/currentContext))
    (slot-based-derivable p (ct/currentContext) nil)))


;;; Printing Proofs

(defn asserted-support-sets
  [ss]
  (let [all-asserted (fn [l] (every? #(ct/asserted? (get-term %) (ct/currentContext)) l))]
    (filter all-asserted (map second ss))))

(defn print-proof-step
  ([result-term msg-support rule-name]
    (when-let [support (first (asserted-support-sets msg-support))]
      (println)
      (println "Since:" (get-term (first support)))
      (doseq [s (rest support)]
        (println "and:" (get-term s)))
      (println "I derived:" result-term "by" rule-name)
      (println)))
  ([result-term msg-support rule-node rule-name]
  ;; Find a support set in msg-support where everything
  ;; is currently believed, and pick it as the one to 
  ;; report.
  (when-let [support (first (asserted-support-sets msg-support))]
    (println)
    (println "Since:" rule-node)
    (doseq [s support]
      (println "and:" (get-term s)))
    (println "I derived:" result-term "by" rule-name)
    (println))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Rules ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn negation-elimination
  "Invert the truth of the :u-true? key in the message, and pass onward."
  [message node]
  ;; new-msgs is used in this case, not because we want to combine messages, but
  ;; because we want to ensure we don't re-produce the same message.
  (let [new-msgs (get-new-messages (@msgs node) message)
        dermsg (derivative-message message
                                   :origin node
                                   :support-set (der-tag (:support-set message))
                                   :u-true? (not (:u-true? message))
                                   :flaggedns {node (:u-true? message)}
                                   :type 'U-INFER)
        uch (@u-channels node)]
    
    (print-debug :infer #{node} (print-str "INFER: negation-elimination" node "\n -- message:" message "\n    -- derived messages:" dermsg))

    (when (and showproofs 
               (not (:u-true? message)))
      (doseq [u uch]
        (send screenprinter (fn [_] (print-proof-step (:destination u) 
                                                      (:support-set message)
                                                      "negation-elimination")))))
    (when (seq new-msgs) 
      (zipmap uch (repeat (count uch) dermsg)))))

(defn consensus-introduction
  "This rule introduces conjunction or negation, since they are extremely similar."
  [message node new-msgs rule-name pos-is-true?]
  (let [true-msgs (if pos-is-true?
                    (filter #(= (:pos %) (count (@u-channels node))) new-msgs)
                    (filter #(= (:neg %) (count (@u-channels node))) new-msgs))
        false-msgs (if pos-is-true?
                     (filter #(pos? (:neg %)) new-msgs)
                     (filter #(pos? (:pos %)) new-msgs))
        dermsg-fn (fn [dermsgs truthval]
                    (into {} (map #(vector % (derivative-message 
                                             message
                                             :origin node
                                             :support-set (if (has-shared-os? (:antecedent-support-sets %))
                                                            (der-tag (:support-set %))
                                                            (ext-tag (:support-set %)))
                                             :type 'I-INFER
                                             :flaggedns {node truthval}))
                                dermsgs)))
        dermsgs-t (dermsg-fn true-msgs true) 
        dermsgs-f (dermsg-fn false-msgs false)
        ich (@i-channels node)]
    
    (print-debug :infer #{node} (print-str "INFER:" (if pos-is-true? "conjunction-introduction" "negation-introduction") 
                                           node "\n -- message:" message "\n    -- True derived messages:" dermsgs-t "\n    -- False derived messages:" dermsgs-f))

    (concat
      (when (seq true-msgs)
        (when showproofs
          (doseq [[tmsg dermsg] dermsgs-t]
            (send screenprinter (fn [_] (print-proof-step node 
                                                          (:support-set tmsg)
                                                          rule-name)))))
        (add-matched-and-sent-messages (@msgs node) (set true-msgs) {:i-channel (set (vals dermsgs-t))})
        (doall
          (for [[_ dermsg] dermsgs-t]
            [true (:support-set dermsg) (zipmap ich (repeat (count ich) dermsg))])))
      (when (seq false-msgs)
        ;; This isn't a perfect solution, but it will stop infinite generation of (not (not ... ))
        ;; TODO: Somehow remove the new-msgs from the matched to allow later generation if needed.
        (when-not (and (= (type-of node) :csneps.core/Negation)
                       (empty? (build/find (list 'not node))))
          (when showproofs
            (doseq [[fmsg dermsg] dermsgs-f]
              (send screenprinter (fn [_] (print-proof-step (build/variable-parse-and-build (list 'not node) :Propositional #{})
                                                            (:support-set fmsg)
                                                            rule-name)))))
          (add-matched-and-sent-messages (@msgs node) (set false-msgs) {:i-channel (set (vals dermsgs-f))})
          (doall
            (for [[_ dermsg] dermsgs-f]
              [false (:support-set dermsg) (zipmap ich (repeat (count ich) dermsg))])))))))

(defn conjunction-introduction
  "We are in an unasserted 'and' node, and would like to know if we now
   can say it is true based on message."
  [message node new-msgs]
  (consensus-introduction message node new-msgs "conjunction-introduction" true))

(defn negation-introduction
  "We are in an unasserted 'nor' node, and would like to know if we now
   can say it is true based on message."
  [message node new-msgs]
  (consensus-introduction message node new-msgs "negation-introduction" false)
  ;; TODO: Also try negation-introduction-reductio.
  )

;;; TODO: Building new contexts and such is probably the task for backward-infer.
(defn negation-introduction-reductio
  "Reductio ad Absurdum"
  [message node]
  (let [new-msgs (get-new-messages (@msgs node) message)]
    
    
    
    
  ))

(defn conjunction-elimination
  "Since the and is true, send a U-INFER message to each of the
   consequents."
  [message node new-msgs]
  (let [dermsg (derivative-message message 
                                   :origin node
                                   :support-set (der-tag (@support node)) ;(conj (:support-set message) node)
                                   :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                   :flaggedns {node true}
                                   :type 'U-INFER)
        uch (@u-channels node)]
    (print-debug :infer #{node} (print-str "INFER: conjunction-elimination" node "\n -- message:" message "\n    -- derived messages:" dermsg))
    (when showproofs
      (doseq [u uch]
        (when (build/pass-message? u dermsg)
          (send screenprinter (fn [_] (print-proof-step (build/apply-sub-to-term (:destination u) (:subst dermsg))
                                                        #{}
                                                        node
                                                        "conjunction-elimination"))))))
    (zipmap uch (repeat (count uch) dermsg))))

(defn numericalentailment-elimination
  "Since the implication is true, send a U-INFER message to each
   of the consequents." 
  [message node new-msgs]
  (when (seq new-msgs)
    ;; If the node only requires 1 antecedent to be true, any incoming positive
    ;; antecedent is enough to fire the rule.
    (if (= (:min node) 1)
      (when (:u-true? message)
        ;(cancel-infer node nil (:taskid message) (:subst message) (:support-set message))
        (let [der-msg (derivative-message message 
                                          :origin node 
                                          :type 'U-INFER 
                                          :support-set (os-union (:support-set message) (@support node))
                                          :u-true? true
                                          :flaggedns {node true}
                                          :taskid (:taskid message)
                                          :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true))]
          (add-matched-and-sent-messages (@msgs node) new-msgs {:u-channel #{der-msg}})
          (when showproofs 
            (doseq [u (@u-channels node)]
              (when (build/pass-message? u der-msg)
                (send screenprinter (fn [_] (print-proof-step (build/apply-sub-to-term (:destination u) (:subst message))
                                                            (:support-set message)
                                                            node
                                                            (str (or 
                                                                   (build/syntype-fsym-map (syntactic-type-of node))
                                                                   "numericalentailment")
                                                                 "-elimination")))))))
        
          (apply conj {} (doall (map #(vector % der-msg) (@u-channels node))))))
      ;; :min > 1
      (let [match-msgs (filter #(when (>= (:pos %) (:min node))
                                 %)
                             new-msgs)
            match-msg (first match-msgs)]
        (when match-msg 
          (let [der-msg (derivative-message match-msg 
                                            :origin node 
                                            :type 'U-INFER 
                                            :support-set (os-union (:support-set message) (@support node))
                                            :u-true? true 
                                            :flaggedns {node true}
                                            :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                            :taskid (:taskid message))]
            ;(cancel-infer node nil (:taskid message) (:subst der-msg) (:support-set der-msg))
            (add-matched-and-sent-messages (@msgs node) (set match-msgs) {:u-channel #{der-msg}})
            (when showproofs 
              (doseq [u (@u-channels node)]
                (when (build/pass-message? u der-msg)
                  (send screenprinter (fn [_] (print-proof-step (build/apply-sub-to-term (:destination u) (:subst message))
                                                            (:support-set match-msg)
                                                            node
                                                            (str (or 
                                                                   (build/syntype-fsym-map (syntactic-type-of node))
                                                                   "numericalentailment")
                                                                 "-elimination")))))))
        
            (apply conj {} (doall (map #(vector % der-msg) (@u-channels node))))))))))

(defn- find-supports-with-without
  [ss with without]
  (filter #(and ((second %) (:name with))
                      (not ((second %) (:name without))))
        ss))

(defn- remove-from-supports
  [ss rem]
  (for [[tag os] ss]
    [tag (disj os (:name rem))]))

(defn numericalentailment-introduction
  ""
  [message node]
  (when (= (count (first (@down-cableset node))) 1)
    (let [ant (ffirst (@down-cableset node))
          cqs (second (@down-cableset node))
          s? (first (find-supports-with-without (:support-set message) ant node))]
      (when s?
        (let [new-msgs (get-new-messages (@msgs node) message)
              match-msgs (filter #(when (= (:pos %) (count cqs)) %) new-msgs)
              match-supports (apply union (map #(:support-set %) match-msgs))
              good-supports (find-supports-with-without match-supports ant node)
              adjusted-supports (set (remove-from-supports good-supports ant))]
          (when match-msgs
            (dosync (alter-support node (os-concat (@support node) adjusted-supports)))
            (let [imsg (derivative-message message
                                           :origin node
                                           :support-set adjusted-supports
                                           :flaggedns {node true}
                                           :type 'I-INFER)]
              (add-matched-and-sent-messages (@msgs node) (set match-msgs) {:i-channel #{imsg}})
              (doseq [cqch (@i-channels node)] 
                (submit-to-channel cqch imsg)))
            (when showproofs
              (send screenprinter (fn [_] (print-proof-step node 
                                                            adjusted-supports
                                                            "if-introduction"))))))))))

(defn andor-elimination
  "Since the andor is true, we may have enough information to do elimination
   on it. "
  [message node new-msgs]
  (let [new-ruis new-msgs
        totparam (totparam node)
        pos-matches (filter #(= (:pos %) (:max node)) new-ruis)
        neg-matches (filter #(= (- totparam (:neg %)) (:min node)) new-ruis)]
    (print-debug :rui #{node} (print-str "NEW RUI: Andor Elimination" new-ruis "at" node))
    ;(send screenprinter (fn [_]  (println "Pos" pos-matches) (println "Neg" neg-matches)))
    
    (merge 
      (when (seq pos-matches)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                    :origin node 
                                                                    :type 'U-INFER 
                                                                    :u-true? false 
                                                                    :flaggedns {node true}
                                                                    :support-set (os-union (:support-set %) (@support node))
                                                                    :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                                                    :taskid (:taskid message))) 
                                     pos-matches))]
          
          (when showproofs
            (doseq [[pos-match der-msg] der-msgs
                    u (@u-channels node)]
              (when (and (not ((:flaggedns pos-match) (:destination u)))
                         (not (negated? (:destination u)))
                         (build/pass-message? u der-msg))
                (send screenprinter (fn [_] (print-proof-step (build/build (list 'not (build/apply-sub-to-term (:destination u) (:subst pos-match)))
                                                                           :Proposition {} #{})
                                                              (:support-set pos-match)
                                                              node
                                                              (str (or 
                                                                     (build/syntype-fsym-map (syntactic-type-of node))
                                                                     "andor")
                                                                   "-elimination (1)")))))))

        (add-matched-and-sent-messages (@msgs node) (set pos-matches) {:u-channel (set (vals der-msgs))} false)
                
        (into {} (for [[pos-match der-msg] der-msgs
                       u (@u-channels node)
                       :when (and (nil? ((:flaggedns pos-match) (:destination u)))
                                  (not (negated? (:destination u))))]
                   [u der-msg]))))
      
      (when (seq neg-matches)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                    :origin node 
                                                                    :type 'U-INFER 
                                                                    :u-true? true
                                                                    :flaggedns {node true}
                                                                    :support-set (os-union (:support-set %) (@support node))
                                                                    :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                                                    :taskid (:taskid message))) 
                                     neg-matches))]

          (when showproofs 
            (doseq [[neg-match der-msg] der-msgs
                    u (@u-channels node)]
              (when (and (not ((:flaggedns neg-match) (:destination u)))
                         (not (negated? (:destination u)))
                         (or (build/valve-open? u) (build/pass-message? u der-msg)))
                (send screenprinter (fn [_] (print-proof-step (build/build (list 'not (build/apply-sub-to-term (:destination u) (:subst neg-match)))
                                                                           :Proposition {} #{})
                                                              (:support-set neg-match)
                                                              node
                                                              (str (or 
                                                                     (build/syntype-fsym-map (syntactic-type-of node))
                                                                     "andor")
                                                                   "-elimination (2)")))))))

        (add-matched-and-sent-messages (@msgs node) (set neg-matches) {:u-channel (set (vals der-msgs))} false)
                
        (into {} (for [[neg-match der-msg] der-msgs
                       u (@u-channels node)
                       :when (and (nil? ((:flaggedns neg-match) (:destination u)))
                                  (not (negated? (:destination u))))]
                   [u der-msg])))))))

;     Inference can terminate
;        as soon as one of the following is determined to hold:
;        (1) The number of args asserted/derived is > max
;            or the number of negated args asserted/derived is > (tot-min)
;        (2) The number of args asserted/derived is >= min
;            and the number of negated args asserted/derived is >= (tot-max)
;     If type is andor, in case (1) the derivation fails,
;                       and in case (2) the derivation succeeds.
;     If type is thresh, in case (1) the derivation succeeds,
;                        and in case (2) the derivation fails.
(defn param2op-introduction
  "Check the RUIs to see if I have enough to be true."
  [message node new-msgs]
  (let [totparam (totparam node)
        case1s (filter #(or (> (:pos %) (:max node))
                            (> (:neg %) (- totparam (:min node)))) new-msgs)
        case2s (filter #(and (>= (:pos %) (:min node))
                             (>= (:neg %) (- totparam (:max node)))) new-msgs)
        ich (@i-channels node)]
    (cond
      (isa? (syntactic-type-of node) :csneps.core/Andor)
      (when (seq case2s)
        (let [dermsgs (into {} (map #(vector % (derivative-message 
                                                 message 
                                                 :origin node
                                                 :support-set (if (has-shared-os? (:antecedent-support-sets %))
                                                                (der-tag (:support-set %))
                                                                (ext-tag (:support-set %)))
                                                 :type 'I-INFER
                                                 :flaggedns {node true}
                                                 :u-true? true))
                                    case2s))]
          (when showproofs 
            (doseq [[case2 dermsg] dermsgs]
              (send screenprinter (fn [_] (print-proof-step node
                                                            (:support-set case2)
                                                            (str (or 
                                                                   (build/syntype-fsym-map (syntactic-type-of node))
                                                                   "param2op")
                                                                 "-introduction"))))))
          (doall 
            (for [[_ dermsg] dermsgs]
              [true (:support-set dermsg) (zipmap ich (repeat (count ich) dermsg))]))))
      (isa? (syntactic-type-of node) :csneps.core/Thresh)
      (when (seq case1s)
        (let [dermsgs (into {} (map #(vector % (derivative-message 
                                                 message 
                                                 :origin node
                                                 :support-set (if (has-shared-os? (:antecedent-support-sets %))
                                                                (der-tag (:support-set %))
                                                                (ext-tag (:support-set %)))
                                                 :type 'I-INFER
                                                 :flaggedns {node true}
                                                 :u-true? true))
                                    case1s))]
          (when showproofs 
            (doseq [[case1 dermsg] dermsgs]
              (send screenprinter (fn [_] (print-proof-step node
                                                            (:support-set case1)
                                                            (str (or 
                                                                   (build/syntype-fsym-map (syntactic-type-of node))
                                                                   "param2op")
                                                                 "-introduction"))))))
          (doall 
            (for [[_ dermsg] dermsgs]
              [true (:support-set dermsg) (zipmap ich (repeat (count ich) dermsg))])))))))
  
(defn thresh-elimination
  "Thesh is true if less than min or more than max."
  [message node new-msgs]
  (let [totparam (totparam node)
        ;; Case 1: There are >= minimum true. Therefore > maximum must be true. 
        ;; If there are totparam - max - 1 false, then we can make the rest true.
        more-than-min-true-match (filter #(and (>= (:pos %) (:min node))
                                               (= (:neg %) (- totparam (:max node) 1)))
                                         new-msgs)
        ;; Case 2: There are enough false cases such that maximum could not be true.
        ;; Therefore the minimum must be true. If enough of them are already, the rest
        ;; are false.
        less-than-max-true-match (filter #(and (>= (:neg %) (- totparam (:max node)))
                                               (= (:pos %) (dec (:min node))))
                                          new-msgs)]
    (merge
      (when (seq more-than-min-true-match)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                   :origin node 
                                                                   :type 'U-INFER 
                                                                   :u-true? true 
                                                                   :flaggedns {node true}
                                                                   :support-set (os-union (:support-set %) (@support node))
                                                                   :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                                                   :taskid (:taskid message)))
                                     more-than-min-true-match))]
        
          (when showproofs 
            (doseq [[more-than-min-true-match der-msg] der-msgs
                    u (@u-channels node)
                    :when (and (not ((:flaggedns more-than-min-true-match) (:destination u)))
                               (build/pass-message? u der-msg))
                    :let [result-term (build/apply-sub-to-term (:destination u) (:subst more-than-min-true-match))]]
              (send screenprinter (fn [_] (print-proof-step result-term
                                                            (:support-set more-than-min-true-match)
                                                            node
                                                            (str (or 
                                                                   (build/syntype-fsym-map (syntactic-type-of node))
                                                                   "thresh")                                                               
                                                                 "-elimination"))))))
          
          (add-matched-and-sent-messages (@msgs node) (set more-than-min-true-match) {:u-channel (set (vals der-msgs))} false)
          
          (into {} (for [[more-than-min-true-match der-msg] der-msgs
                         u (@u-channels node)
                         :when (not ((:flaggedns more-than-min-true-match) (:destination u)))]
                     [u der-msg]))))
      
      (when (seq less-than-max-true-match)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                   :origin node 
                                                                   :type 'U-INFER 
                                                                   :u-true? true 
                                                                   :flaggedns {node true}
                                                                   :support-set (os-union (:support-set %) (@support node))
                                                                   :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                                                   :taskid (:taskid message)))
                                     less-than-max-true-match))]
        
          (when showproofs 
            (doseq [[less-than-max-true-match der-msg] der-msgs
                    u (@u-channels node)
                    :when (and (not ((:flaggedns less-than-max-true-match) (:destination u)))
                               (build/pass-message? u der-msg))
                    :let [result-term (build/apply-sub-to-term (:destination u) (:subst less-than-max-true-match))]]
              (send screenprinter (fn [_] (print-proof-step result-term 
                                                            (:support-set less-than-max-true-match)
                                                            node
                                                            (str (or 
                                                                   (build/syntype-fsym-map (syntactic-type-of node))
                                                                   "thresh")                                                               
                                                                 "-elimination"))))))
          
          (add-matched-and-sent-messages (@msgs node) (set less-than-max-true-match) {:u-channel (set (vals der-msgs))} false)
          
          (into {} (for [[less-than-max-true-match der-msg] der-msgs
                         u (@u-channels node)
                         :when (not ((:flaggedns less-than-max-true-match) (:destination u)))]
                     [u der-msg])))))))

(defn g-chan-to-node?
  [orig dest]
  (some #(= (:destination %) dest) (@g-channels orig)))

(defn whquestion-infer 
  [message node]
  (if (g-chan-to-node? (:origin message) node)
    ;; Msgs from restrictions.
    (let [new-combined-messages (get-new-messages (@msgs node) message)
          querytermct (count (filter queryTerm? (build/flatten-term node)))
          rel-combined-messages (when new-combined-messages
                                  (filter #(= (:pos %) querytermct) new-combined-messages))]
      (doseq [rcm rel-combined-messages
              :let [instance (build/find-term-with-subst-applied node (:subst rcm))]]
        ;; {subst inst} added, even if inst is nil! We'll find it later if it shows up.
        (dosync (alter instances assoc node (assoc (@instances node) (:subst rcm) instance)))))
    ;; Msgs from unifiers.
    (when (and (@instances node)
            (nil? ((@instances node) (:subst message)))) 
      ;; The instance we have received may not be built yet.
      (let [instance (or (build/find-term-with-subst-applied node (:subst message))
                         (build/apply-sub-to-term node (:subst message)))]
        (dosync (alter instances assoc node (assoc (@instances node) (:subst message) instance))))))
      
  (when (analyticTerm? node) ;; This is a restriction.
    (let [imsg (derivative-message message :origin node :flaggedns {node true})]
      (add-matched-and-sent-messages (@msgs node) #{(sanitize-message message)} {:i-channel #{imsg} :g-channel #{imsg}}) ;; Just gchans?
      (doseq [cqch (@i-channels node)] 
        (submit-to-channel cqch imsg)))))
    
              
  
  
;  
;    (when debug (send screenprinter (fn [_]  (println "Deriving answer:" (:subst message)))))
;
;    (dosync (alter instances assoc node (assoc (@instances node) (build/apply-sub-to-term node (:subst message) true) (:subst message)))))
  
  
    ;(dosync (alter (@instances node) assoc (build/apply-sub-to-term node (:subst message) true) (:subst message))))

(defn policy-instantiation
  ;; Really, a kind of elimination rule.
  [message node new-msgs]
  ;; Before we do anything, we check that the received message fully instantiates
  ;; at least the originator with a term actually in the KB and is believed. If not,
  ;; discard it. We have to do this because unlike normal rules of inference, this rule
  ;; actually needs to know that's what it's receiving is asserted (the results won't have an OS
  ;; to make them contingent on the context). 
  (when (ct/asserted? (build/find-term-with-subst-applied (:origin message) (:subst message)) (ct/currentContext))
    (let [inchct (count (@ant-in-channels node)) ;; Should work even with sub-policies.
                                                  ;; What about shared sub-policies though?
          inst-msgs (filter #(= (:pos %) inchct) new-msgs)
          der-msgs (map #(derivative-message % 
                                             :origin node 
                                             :type 'I-INFER 
                                             :taskid (:taskid message)
                                             :flaggedns {node true}
                                             :support-set (os-union (:support-set %) (@support node))) 
                        inst-msgs)
          ich (@i-channels node)]

      (when (seq der-msgs)
        (print-debug :infer #{node} (print-str "INFER: policy-instantiation" node "\n -- message:" message "\n    -- derived messages:" der-msgs))
        (add-matched-and-sent-messages (@msgs node) (set inst-msgs) {:i-channel (set der-msgs)}))
      (apply concat 
             (for [nmsg der-msgs] 
               (zipmap ich (repeat (count ich) nmsg)))))))

(defn apply-to-all-restrictions
  [subst arb]
  (let [rsts (@restriction-set arb)]
    (map #(build/apply-sub-to-term % subst) rsts)))

;;; A generic which has had all of the restrictions of its arbitraries
;;; met, should produce an instance (and resulting messages) for those
;;; restrictions. That instance has origin sets which indicate that it 
;;; could be inferred in two ways (in addition to itself being asserted): 
;;; 1) The generic is believed, and all of the restrictions are believed.
;;; 2) All of the restrictions are believed, and the instance is believed.
;;; These cases are not checked by the generic - that is the job of the
;;; origin set, which the resulting instance will have.
(defn generic-infer
  [message node]
  ;; Instance from g-channel
  (if (g-chan-to-node? (:origin message) node)
    (let [new-combined-messages (get-new-messages (@msgs node) message)
          rel-combined-messages (when new-combined-messages
                                  (filter #(> (:pos %) 0) new-combined-messages))]

      (doseq [rcm rel-combined-messages]
        
        (let [instance (if (:fwd-infer message)
                         (build/apply-sub-to-term node (:subst rcm))
                         (build/find-term-with-subst-applied node (:subst rcm)))
              inst-support ;(os-concat 
                             (os-union (:support-set rcm) (@support node))
                             ;; This can't be minimal, can it? DRS 7/5/14
                             ;; Would seem (@support instance) would always be more minimal.
                             ;(os-union (:support-set rcm) (@support instance)))
              ;;; Next line is experimental! Not sure it's needed in this case. 
              expanded-subst (build/expand-substitution node (:subst rcm))
              der-msg (derivative-message rcm
                                         :origin node
                                         :support-set inst-support
                                         :subst expanded-subst
                                         :u-true? true
                                         :flaggedns {node true}
                                         :type 'I-INFER
                                         :fwd-infer? (:fwd-infer? message)
                                         :taskid (:taskid message))]
          
          ;(when instance 
            ;(println instance (os-union (:support-set rcm) (@support instance)))
            ;(dosync 
              ;(alter support assoc instance (os-concat (@support instance) inst-support))
             ; (alter instances assoc node (assoc (@instances node) instance (:subst rcm)))))
          
          ;; Don't remove matched messages from working to avoid cases where two arbs need
          ;; matching, but they independently get removed from working. There is likely an
          ;; optomization possible where they can be removed if pos has some value. 
          (add-matched-and-sent-messages (@msgs node) #{rcm} {:i-channel #{der-msg} :g-channel #{der-msg}} false)
          
          (when (and showproofs instance 
                     (ct/asserted? node (ct/currentContext)) 
                     (filter #(build/pass-message? % der-msg) (union (@i-channels node) (@g-channels node))))
            (print-debug :os #{node} (print-str "OS: message:" (:support-set rcm) "node:" (@support node) "inst-support:" inst-support))
            (send screenprinter (fn [_] (print-proof-step instance
                                                          (:support-set rcm)
                                                          node
                                                          "generic-instantiation (1)"))))
          (doseq [ch (union (@i-channels node) (@g-channels node))]
            (if (and instance (analyticTerm? instance))
              nil
              (submit-to-channel ch der-msg))))))
    ;; Instance from unifying term.
    (let [instance (:origin message)]
      (when-not (seen-message? (@msgs node) message) ;; Don't bother if we've already seen this message.
        (let [subst-support (set 
                              (map (fn [t] (:name t)) 
                                   (flatten 
                                     (map (fn [a] (apply-to-all-restrictions (:subst message) a))
                                          (filter #(arbitraryTerm? %) 
                                                  (keys (:subst message)))))))
              ;;; Next line is experimental! Not sure how it overlaps with subst-support, if it all.
              expanded-subst (build/expand-substitution node (:subst message))
              outgoing-support (if (seq subst-support)
                                 (os-union (:support-set message)
                                           #{['der subst-support]})
                                 (:support-set message))
              der-msg (derivative-message message
                                          :origin node
                                          :support-set outgoing-support
                                          :subst expanded-subst
                                          :type 'I-INFER
                                          :flaggedns {node true}
                                          :fwd-infer? (:fwd-infer? message)
                                          :taskid (:taskid message))]
          
      ;(send screenprinter (fn [_] (println "!!!" message (:subst message) outgoing-support)))
       (add-matched-and-sent-messages (@msgs node) #{(sanitize-message message)} {:i-channel #{der-msg} :g-channel #{der-msg}})
       (dosync (alter instances assoc node (assoc (@instances node) instance (:subst message))))
       (when (and showproofs 
                  (ct/asserted? node (ct/currentContext)) 
                  (filter #(build/pass-message? % der-msg) (union (@i-channels node) (@g-channels node))))
         (send screenprinter (fn [_] (print-proof-step instance
                                                       outgoing-support
                                                       node
                                                       "generic-instantiation (2)"))))
       (doseq [ch (union (@i-channels node) (@g-channels node))]
         (submit-to-channel ch der-msg)))))))
              
    
(defn arbqvar-instantiation
  [message node]
  (print-debug :der #{node} (print-str "DER: Arbitrary derivation:" (:subst message) "at" node "msg" message))    
  (when (seq (:subst message)) ;; Lets ignore empty substitutions for now.
    (let [new-ruis (get-new-messages (@msgs node) message)
          resct (count (@restriction-set node))
          der-msg-t (filter #(= (:pos %) resct) new-ruis)
          new-msgs (map #(derivative-message % 
                                             :origin node 
                                             :type 'I-INFER 
                                             :taskid (:taskid message) 
                                             :flaggedns {node true}
                                             :fwd-infer? (:fwd-infer? message)) der-msg-t)
          gch (@g-channels node)]
      (print-debug :rui #{node} (print-str "NEW RUI: Arb/Qvar" new-ruis "\n -- at" node "\n   -- via new message" message))
      (when (seq der-msg-t)
;        (send screenprinter (fn [_]  (println "NEWMESSAGE:" (count (for [msg new-msgs
;                                                                         ch gch]
;                                                                     [ch msg]))
;                                              new-msgs)))

        (add-matched-and-sent-messages (@msgs node) (set der-msg-t) {:g-channel (set new-msgs)})

        (print-debug :newmsg #{node} (print-str "NEWMESSAGE:" new-msgs "at" node))
        [true (for [msg new-msgs
                    ch gch]
                [ch msg])]))))

(defn instantiate-indefinite
  "Takes an indefinite and substitution, and returns a new
   indefinite which has as its restrictions:
   1) applies subst to every restriction of ind
   2) applies subst to every restriction of each of ind's deps
   and no dependencies."
  [ind subst]
  (let [new-rsts (loop [new-rsts (map #(build/apply-sub-to-term % subst) (@restriction-set ind))
                        deps (@dependencies ind)]
                   (if (empty? deps)
                     new-rsts
                     (recur 
                       (concat new-rsts (map #(build/apply-sub-to-term % subst) (@restriction-set (first deps))))
                       (rest deps))))
        ;; TODO FIX:
        new-ind (new-indefinite :name (symbol (str "ind" (ind-counter)))
                                       :var-label (:var-label ind)
                                       :msgs (create-message-structure :csneps.core/Indefinite nil)
                                       :restriction-set (ref (set new-rsts)))]
    (inc-ind-counter)
    (instantiate-sem-type (:name new-ind) (semantic-type-of ind))
    new-ind))

(defn indefinite-instantiation
  [message node]
  ;; Only do something if this came from a dep of the ind.
  (when (and ((@dependencies node) (:origin message))
             (seq (:subst message)))
    (let [new-ruis (get-new-messages (@msgs node) message)
          depct (count (@dependencies node))
          der-rui-t (filter #(= (:pos %) depct) new-ruis)
          new-msgs (map #(derivative-message 
                           % 
                           :origin node 
                           :taskid (:taskid message)
                           :flaggedns {}
                           :subst (assoc 
                                    (:subst message) 
                                    node 
                                    (instantiate-indefinite node (:subst message)))) der-rui-t)
          gch (@g-channels node)]
      (when (seq der-rui-t)
        (print-debug :newmsg #{node} (print-str "NEWMESSAGE:" new-msgs "at" node))
        [true (for [msg new-msgs
                    ch gch]
                [ch msg])]))))

;; When a message reaches a closure, the result is that same message,
;; with substitutions for the closed variables removed. 
(defn closure-elimination
  [message node]
  (let [result-msg (derivative-message message
                                       :origin node
                                       :subst (apply dissoc (:subst message) (map #(:var-label %) (:closed-vars node)))
                                       :support-set (os-union (:support-set message) (:support node)))
        ich (@i-channels node)]
    (zipmap ich (repeat (count ich) result-msg))))

(defn elimination-infer
  "Input is a message and node, output is a set of messages derived."
  [message node new-msgs]
  (print-debug :infer #{node} (print-str "INFER: (elim) Inferring in:" node))
  (case (type-of node)
    :csneps.core/CARule (policy-instantiation message node new-msgs)
    :csneps.core/Conjunction (conjunction-elimination message node new-msgs)
    (:csneps.core/Numericalentailment
     :csneps.core/Implication) (numericalentailment-elimination message node new-msgs)
    (:csneps.core/Andor 
     :csneps.core/Disjunction 
     :csneps.core/Xor
     :csneps.core/Nand)  (andor-elimination message node new-msgs)
    (:csneps.core/Thresh
     :csneps.core/Equivalence) (thresh-elimination message node new-msgs)
    :csneos.core/Closure (closure-elimination message node new-msgs)
    nil))

(defn introduction-infer
  ""
  [message node new-msgs]
  (print-debug :infer #{node} (print-str "INFER: (intro) Inferring in:" node))
  (case (type-of node)
    :csneps.core/Conjunction (conjunction-introduction message node new-msgs)
    :csneps.core/Negation (negation-introduction message node new-msgs)
    (:csneps.core/Andor 
     :csneps.core/Disjunction 
     :csneps.core/Xor
     :csneps.core/Nand
     :csneps.core/Thresh
     :csneps.core/Equivalence)  (param2op-introduction message node new-msgs)
    nil))

(defn initiate-node-task
  [term message]
  (print-debug :infer #{term} (print-str "INFER: beginning node task" "\n -- on message" message "\n   -- at term" term))

  (when (:fwd-infer? message)
    (dosync (alter future-fw-infer assoc term (union (@future-fw-infer term) (:invoke-set message)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ---- U-INFER Rules ---- ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; If I have just received a U-INFER message, I must make myself
  ;; either true or false according to the message, report that
  ;; new belief, and attempt elimination.
  (when (and (= (:type message) 'U-INFER)
             (not (seen-message? (@msgs term) message)))
    (let [result-term (if (:u-true? message)
                        (build/apply-sub-to-term term (:subst message))
                        (build/build (list 'not (build/apply-sub-to-term term (:subst message))) :Proposition {} #{}))]
      
      (when-not (:fwd-infer? message) (cancel-infer result-term nil (:taskid message) (:subst message) (:support-set message)))
      
      ;; When this hasn't already been derived otherwise in this ct, let the user know.
      (when (and print-intermediate-results
                 (or (:fwd-infer? message)
                     (not (ct/asserted? result-term (ct/currentContext)))))
        (send screenprinter (fn [_]  (println "> " result-term))))
      
      ;; Update origin sets of result appropriately.
      (dosync (alter-support result-term (os-concat (@support result-term) (:support-set message))))
      
      ;; If this is a cq of a rule we are introducing by numerical entailment
      ;; let the rule know.
      (let [ucs-map (@up-cablesetw term)
            cq-ucs (when ucs-map (ucs-map (slot/find-slot 'cq)))
            cq-ucs (when cq-ucs @cq-ucs)]
        (doseq [rule cq-ucs]
          (numericalentailment-introduction message rule)))
      
      ;; Send messages onward that this has been derived.
      (if-not (= :csneps.core/Negation (type-of term))
        (let [imsg (derivative-message message
                                     :origin term
                                     :support-set (@support result-term)
                                     :flaggedns {term (:u-true? message)}
                                     :type 'I-INFER)]
          ;; Save the message for future terms which might have channels
          ;; from this. Sometimes necessary for forward focused reasoning.
          (when (@msgs term) (add-matched-and-sent-messages (@msgs term) #{(sanitize-message message)} {:i-channel #{imsg}}))
          ;; Do the sending.
          (doseq [cqch (@i-channels term)] 
            (submit-to-channel cqch imsg)))
        ;; Negations are special
        (when-let [result (negation-elimination message term)]
          (doseq [[ch msg] result] 
            (submit-to-channel ch msg))))
      
      ;; If I've now derived the goal of a future bw-infer process, it can be cancelled.
      (when (get @(:future-bw-infer term) result-term)
        (cancel-infer-of term)))
    
    (when (:u-true? message)
      ;; Elimination after a U-INFER message is different. 
      ;; Need to look at already matched messages, and combine with the new support, and relay that. 
      ;; Since we reason in all contexts, it won't result in any new derivations, just new reasons for old ones.
      ;; Each of the elimination rules should have a condition for U-INFER messages to do this.
      (when-let [result (elimination-infer message term (get-matched-messages (@msgs term)))]
        (when (:infer @debug-features) (send screenprinter (fn [_]  (println "INFER: Result Inferred " result))))
        (doseq [[ch msg] result] 
          (submit-to-channel ch msg)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ---- I/U-INFER Rules ---- ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; I have an old note to myself that this should be I/U. I'm not sure the rationale for that.
  ;; I think it should be just I-INFER.
  ;; Generic inference.
  (when 
    (and 
      (= (:type message) 'I-INFER)
      (genericTerm? term)
      (not (analyticTerm? term))
      (seq (:subst message)))
    (generic-infer message term))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ---- I-INFER Rules ---- ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; I-INFER can result in both introduction and elimination.
  
  (when (and (= (:type message) 'I-INFER)
             (or (not (@msgs term))
                 (not (seen-message? (@msgs term) message))))
    (cond 
      ;; Actions should execute their primative action.
      (and
        (= (semantic-type-of term) :Action)
        (primaction term))
      ((primaction term) (:subst message))
      ;; "Introduction" of a WhQuestion is really just collecting answers.
      (and (@property-map term) ((@property-map term) :WhQuestion))
      (whquestion-infer message term)
      ;; AnalyticGeneric terms need to just forward the messages
      ;; on towards the variable term.
      (analyticTerm? term)
      (let [imsg (derivative-message message :origin term :flaggedns {term true})]
        (when (@msgs term) (add-matched-and-sent-messages (@msgs term) #{(sanitize-message message)} {:i-channel #{imsg} :g-channel #{imsg}}))
        (when (:infer @debug-features) (send screenprinter (fn [_]  (println "INFER: AnalyticGeneric" term "forwarding message."))))
        (doseq [cqch (@i-channels term)] 
          (submit-to-channel cqch imsg)))
      ;; Arbs
      (or (arbitraryTerm? term) (queryTerm? term))
      (if-let [[true? result] (arbqvar-instantiation message term)]
        (when result 
          (doseq [[ch msg] result] 
            (submit-to-channel ch msg)))
        ;; When introduction fails, try backward-in-forward reasoning. 
        (when (:fwd-infer? message)
          (backward-infer term #{term} nil)))
      ;; Specific instance building.
      (and 
        (genericTerm? (:origin message))
        (not (rule? term))
        (not (carule? term))
        (not (variableTerm? term)))
      (do 
        (when (:u-true? message)
          (dosync (alter-support term (os-concat (@support term) (:support-set message)))))
        ;; Send this new info onward
        (let [imsg (derivative-message message
                                       :origin term
                                       :support-set (:support-set message)
                                       :flaggedns {term true}
                                       :type 'I-INFER)]
          ;; Save the message for future terms which might have channels
          ;; from this. Sometimes necessary for forward focused reasoning.
          (when (@msgs term) (add-matched-and-sent-messages (@msgs term) #{(sanitize-message message)} {:i-channel #{imsg}}))
          ;; Do the sending.
          (doseq [cqch (@i-channels term)] 
            (when (not= (:destination cqch) (:orign message)) ;; Don't just send it back where it came from.
              (submit-to-channel cqch imsg)))))
      ;; Otherwise...
      :else
      (let [new-msgs (get-new-messages (@msgs term) message)]
        ;; Try elimination
        (when-let [result (elimination-infer message term new-msgs)]
          (when (:infer @debug-features) (send screenprinter (fn [_]  (println "INFER: Result Inferred " result))))
          (doseq [[ch msg] result] 
            (submit-to-channel ch msg)))
        ;; Then try introduction
        (let [results (introduction-infer message term new-msgs)]
          (if (seq results)
            (doseq [[true? spt result] results]
              (when (:infer @debug-features) (send screenprinter (fn [_]  (println "INFER: Result Inferred " result "," spt "," true?))))
              ;; Update support.
              (let [resterm (if true?
                              term
                              (build/variable-parse-and-build (list 'not term) :Propositional #{}))]
                (dosync (alter-support resterm (os-concat (@support resterm) spt)))
                (when print-intermediate-results (println "> " resterm)))
              ;; Send results.
              (doseq [[ch msg] result] 
                (submit-to-channel ch msg)))
            ;; When introduction fails, try backward-in-forward reasoning. 
            (when (:fwd-infer? message)
              (backward-infer term #{term} nil)))))))

  (when (@infer-status (:taskid message))
    (.decrement ^CountingLatch (@infer-status (:taskid message)))))
          
  
;;; Message Handling ;;;

(defn create-message-structure
  "Create the RUI structure for a rule node. Chooses to 
   create P-Trees and S-Indexes as necessary."
  [syntype dcs & {:keys [n]}]
  (cond
    (empty? (filter arbitraryTerm? (build/flatten-term dcs)))
    (make-linear-msg-set)
    (and (or (= syntype :csneps.core/Numericalentailment)
             (= syntype :csneps.core/Implication))
         (= n (count (first dcs))))
    (make-ptree syntype dcs)
    (or (= syntype :csneps.core/Conjunction))
    (make-ptree syntype dcs)
    :else
    (make-linear-msg-set)))

(build/fix-fn-defs submit-to-channel blocking-submit-to-channel submit-assertion-to-channels new-message create-message-structure get-sent-messages backward-infer forward-infer add-matched-and-sent-messages)

;;; Reductio

(defn reductio-infer
  [term context]
  (if (ct/asserted? term context)
    term
    ;; Assume the opposite.
    (let [reductio-term (build/build (list 'not term) :Proposition #{})
          ;; If the opposite is already assumed, don't bother doing it again.
          new-context (if (ct/asserted? reductio-term context)
                        context
                        (ct/defineContext (gensym "ct") :parents [context] :hyps [reductio-term]))]
      ;; Forward infer on the reductio-term.
      (forward-infer reductio-term)
      ;; Check if new-context is now inconsistent. If so, infer term. Otherwise, don't.
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User-oriented functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn backward-infer-derivable [term context]
  (let [taskid (gensym "task")]
    (backward-infer term taskid)
    (.await ^CountingLatch (@infer-status taskid))
    (if (ct/asserted? term context)
      #{term}
      #{})))

(defn backward-infer-answer
  "Used for answering WhQuestions"
  [ques context]
  (let [taskid (gensym "task")]
    (backward-infer ques taskid)
    (.await ^CountingLatch (@infer-status taskid))
    (into {} (filter #(ct/asserted? (second %) context) (@instances ques)))))

(defn cancel-infer-of [term]
  (let [term (if (seq? term)
               (build/build term :Proposition #{})
               (get-term term))]
    (when-not term (error "Term not found: " term))
    (cancel-infer term term)))

(defn cancel-infer-from [term]
  (let [term (if (seq? term)
               (build/build term :Proposition #{})
               (get-term term))]
    (when-not term (error "Term not found: " term))
    (cancel-forward-infer term term)))

(defn cancel-focused-infer []
  (dosync
    (alter future-fw-infer empty))
  (doseq [t (vals @TERMS)]
    (dosync (alter (:future-bw-infer t) empty))))

(defn- print-valve [ch]
  (let [selectors-this-ct (filter #(clojure.set/subset? @(:hyps (ct/find-context (second %))) @(:hyps (ct/currentContext))) @(:valve-selectors ch))]
    (cond
      @(:valve-open ch) "-"
      (seq selectors-this-ct) (str "~" (print-str (map #(first %) selectors-this-ct)) "~")
      :else "/")))

(defn ig-status []
  (doseq [x (sort-by first @csneps.core/TERMS)]
    (doseq [s (@semtype-in-channels (second x))]
      (println "X [semtype] -I-" (count @(:waiting-msgs s)) (print-valve s) "->" (:destination s)))
    (doseq [i (@i-channels (second x))]
      (println (:originator i) "-I-" (count @(:waiting-msgs i)) (print-valve i) "->" (:destination i)))
    (doseq [u (@u-channels (second x))]
      (println (:originator u) "-U-" (count @(:waiting-msgs u)) (print-valve u) "->" (:destination u)))
    (doseq [g (@g-channels (second x))]
      (println (:originator g) "-G-" (count @(:waiting-msgs g)) (print-valve g) "->" (:destination g)))))

(defn print-waiting-msgs 
  ([term] (doseq [i (union (@ant-in-channels term) (@semtype-in-channels term))]
            (println (:name term) " - " @(:waiting-msgs i))))
  ([] (doseq [t (vals (sort-by first @TERMS))]
        (print-waiting-msgs t))))

