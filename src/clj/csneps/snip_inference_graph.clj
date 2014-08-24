(in-ns 'csneps.snip)

(load "snip_beliefrevision")
(load "snip_message")
(load "snip_linear_msg_set")
(load "snip_sindex")
(load "snip_ptree")

;; Tracking inference tasks
(def ^:dynamic taskid 0)

;; For debug:
(def print-intermediate-results false)
(def print-results-on-infer false)
(def debug false)
(def showproofs true)

(declare initiate-node-task create-message-structure get-new-messages open-valve cancel-infer-of)

(defn priority-partial
  [priority f & more] 
  (with-meta (apply partial f more) {:priority priority}))

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

(def cpus-to-use (/ (.availableProcessors (Runtime/getRuntime)) 2))

;; Fixed Thread Pool of size 2 * processors, using queue as it's queue.
(def executorService (ThreadPoolExecutor.
                       cpus-to-use
                       cpus-to-use
                       (Long/MAX_VALUE) TimeUnit/NANOSECONDS queue))
(.prestartAllCoreThreads ^ThreadPoolExecutor executorService)

(defn resetExecutor
  []
  (.clear ^PriorityBlockingQueue queue)
  (.shutdown ^ThreadPoolExecutor executorService)
  (def executorService (ThreadPoolExecutor.
                         cpus-to-use
                         cpus-to-use
                         (Long/MAX_VALUE) TimeUnit/NANOSECONDS queue))
  (.prestartAllCoreThreads ^ThreadPoolExecutor executorService)
  (def infer-status (ref {nil (edu.buffalo.csneps.util.CountingLatch.)})))

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

(defn submit-to-channel
  [^csneps.core.build.Channel channel ^csneps.snip.Message message]
  (when debug (send screenprinter (fn [_]  (println "MSGTX: " message))))
  ;; Filter
  (when ((:filter-fn channel) (:subst message))
    ;; Switch
    (when debug (send screenprinter (fn [_]  (println "SWITCH!: " ((:switch-fn channel) (:subst message))))))
    (let [message (derivative-message message :subst ((:switch-fn channel) (:subst message)))]
      (if (build/pass-message? channel message)
        ;; Process the message immediately. For forward infer, this ammounts to 
        ;; ignoring the status of the valve.
        (do 
          (when (@infer-status (:taskid message))
            ;(send screenprinter (fn [_]  (println "inc-stc" (:taskid message) (derivative-message message :taskid taskid))))
            (.increment ^CountingLatch (@infer-status (:taskid message))))
          (when debug (send screenprinter (fn [_]  (println "MSGRX: " message "by" (:destination channel)))))
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

(defn get-antecedents
  [term]
  (let [slot-map (cf/dcsRelationTermsetMap term)]
    (case (type-of term)
      :csneps.core/Conjunction
      (get slot-map (slot/find-slot 'and))
      (:csneps.core/Andor 
       :csneps.core/Disjunction 
       :csneps.core/Xor
       :csneps.core/Nand)
      (get slot-map (slot/find-slot 'andorargs))
      (:csneps.core/Thresh
       :csneps.core/Equivalence)
      (get slot-map (slot/find-slot 'threshargs))
      (:csneps.core/Numericalentailment
       :csneps.core/Implication)
      (get slot-map (slot/find-slot 'ant))
      nil)))

(defn get-vars
  "Returns the vars in the given term, or, if the term is a rule
   returns the intersection of variables in its antecedents."
  [term]
  (if-let [ants (get-antecedents term)]
    (apply intersection (map #(set (filter build/variable? (build/flatten-term %))) ants))
    (set (filter build/variable? (build/flatten-term term)))))

(defn add-valve-selector
  [channel subst context taskid]
  (let [vars-in-orig (get-vars (:originator channel))
        subst (build/substitution-application-nomerge (merge subst (:filter-binds channel))
                                                      (or (:switch-binds channel) #{}))
        subst (into {} (filter #(vars-in-orig (first %)) subst))
        valve-selector [subst (:name context)]]
    ;; Only add the selector and check for new matching messages if this is actually a new selector.
    ;; New messages will otherwise be checked upon being submitted to the channel.
    (when-not (@(:valve-selectors channel) valve-selector)
     (dosync (commute (:valve-selectors channel) conj valve-selector))
     (doseq [msg @(:waiting-msgs channel)]
       (when (build/pass-vs? valve-selector msg)
         ;(send screenprinter (fn [_]  (println "Pass")))
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
         (.execute ^ThreadPoolExecutor executorService 
           (priority-partial 1 initiate-node-task (:destination channel) (derivative-message msg :taskid taskid))))))
    subst))

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
    (let [vars-in-orig (when subst (get-vars (:originator channel)))
          subst (when subst (build/substitution-application-nomerge (merge subst (:filter-binds channel))
                                                                    (or (:switch-binds channel) #{})))
          subst (when subst (into {} (filter #(vars-in-orig (first %)) subst)))
          ;is-rel-vs? (fn [vs] (some #(clojure.set/subset? 
          ;                             (second %) 
          ;                             (set (map (fn [h] (:name h)) @(:hyps (second vs))))) 
          ;                          hyps))
          is-rel-vs? (fn [vs] (let [ct (ct/find-context (second vs))]
                                (some #(hyp-subst-of-ct? (second %) ct) hyps)))
          rel-vses (filter is-rel-vs? @(:valve-selectors channel))
          match-vses (when subst (filter #(submap? subst (first %)) rel-vses))]
      ;(if (seq rel-vses)
      ;(send screenprinter (fn [_] (println channel vars-in-orig subst "VSes" (map first @(:valve-selectors channel)))));)
      
      (if subst
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
    (doseq [ch (@ant-in-channels term)]
      (when (and (not (visited term)) 
                 (not (visited (:originator ch))))
                 ;(not= (union @(:future-bw-infer (:originator ch)) invoketermset) @(:future-bw-infer (:originator ch))))
        (when debug (send screenprinter (fn [_]  (println "BW: Backward Infer -" depth "- opening channel from" (:originator ch) "to" term "(task" taskid")"))))
        ;(send screenprinter (fn [_]  (println "BW: Backward Infer -" depth "- opening channel from" (:originator ch) "to" term)))
        (let [subst (add-valve-selector ch subst context taskid)]
          ;(send screenprinter (fn [_]  (println "BW: Backward Infer -" depth "- opening channel from" (:originator ch) "to" term "vs" subst)))
          
          ;(open-valve ch taskid)
          ;(send screenprinter (fn [_]  (println "inc-bwi" taskid)))
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
                              taskid)))))))

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
                           (for [ch (@ant-in-channels term)
                                 :let [new-subst (remove-valve-selector ch subst hyps)]
                                 :when (not (nil? new-subst))]
                             [ch new-subst]))]
        (when (seq affected-chs)
          (when debug (send screenprinter (fn [_]  (println "CANCEL: Cancel Infer - closing incoming channels to" term))))
          (doseq [[ch subst] affected-chs]
            (when taskid (.increment ^CountingLatch (@infer-status taskid)))
            (.execute ^ThreadPoolExecutor executorService 
                (priority-partial Integer/MAX_VALUE 
                                  (fn [t c id s h] (cancel-infer t c id s h) (when id (.decrement ^CountingLatch (@infer-status id))))
                                  (:originator ch) 
                                  cancel-for-term 
                                  taskid
                                  subst
                                  hyps)))))));)

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
  "Invert the truth of the :true? key in the message, and pass onward."
  [message node]
  ;; new-msgs is used in this case, not because we want to combine messages, but
  ;; because we want to ensure we don't re-produce the same message.
  (let [new-msgs (get-new-messages (@msgs node) message)
        dermsg (derivative-message message
                                   :origin node
                                   :support-set (der-tag (:support-set message))
                                   :true? (not (:true? message))
                                   :type 'U-INFER)
        uch (@u-channels node)]

    (when (and showproofs 
               (not (:true? message)))
      (doseq [u uch]
        (send screenprinter (fn [_] (print-proof-step (:destination u) 
                                                      (:support-set message)
                                                      "negation-elimination")))))
    (when (seq new-msgs) 
      (zipmap uch (repeat (count uch) dermsg)))))

;(defn negation-introduction
;  "Pretty much conjunction-introduction, but with :neg instead of :pos"
;  [message node]
;  (let [new-ruis (get-new-messages (:msgs node) message)
;        der-rui (some #(= (:neg %) (count @(:u-channels node))) new-ruis)
;        dermsg (derivative-message message
;                                   :origin node
;                                   :support-set (conj (:support-set message) node) ;; TODO: THIS IS WRONG
;                                   :true? true
;                                   :type 'I-INFER)
;        ich @(:i-channels node)]
;    (when debug (send screenprinter (fn [_]  (println "N-Int" new-ruis "\n" der-rui))))
;    (when der-rui
;      (when showproofs 
;        (send screenprinter (fn [_] (println "I derived: " node " by negation-introduction"))))
;      (dosync (alter (:support node) conj (:support-set message)))
;      [true nil (zipmap ich (repeat (count ich) dermsg))]))) ;;TODO: Fix this.


;;; TODO: Building new contexts and such is probably the task for backward-infer.
(defn negation-introduction
  "Reductio ad Absurdum"
  [message node]
  (let [new-msgs (get-new-messages (@msgs node) message)]
    
    
    
    
  ))
  

(defn numericalentailment-elimination
  "Since the implication is true, send a U-INFER message to each
   of the consequents." 
  [message node new-msgs]
  (when (seq new-msgs)
    ;; If the node only requires 1 antecedent to be true, any incoming positive
    ;; antecedent is enough to fire the rule.
    (if (= (:min node) 1)
      (when (:true? message)
        ;(cancel-infer node nil (:taskid message) (:subst message) (:support-set message))
        (let [der-msg (derivative-message message 
                                          :origin node 
                                          :type 'U-INFER 
                                          :support-set (os-union (:support-set message) (@support node))
                                          :true? true
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
                                            :true? true 
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
                                           :type 'I-INFER)]
              (add-matched-and-sent-messages (@msgs node) (set match-msgs) {:i-channel #{imsg}})
              (doseq [cqch (@i-channels node)] 
                (submit-to-channel cqch imsg)))
            (when showproofs
              (send screenprinter (fn [_] (print-proof-step node 
                                                            adjusted-supports
                                                            "if-introduction"))))))))))

(defn conjunction-elimination
  "Since the and is true, send a U-INFER message to each of the
   consequents."
  [message node new-msgs]
  (let [dermsg (derivative-message message 
                                   :origin node
                                   :support-set (der-tag (@support node)) ;(conj (:support-set message) node)
                                   :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                   :type 'U-INFER)
        uch (@u-channels node)]
    (when debug (send screenprinter (fn [_]  (println "ELIMINATING"))))
    (when showproofs
      (doseq [u uch]
        (when (build/pass-message? u dermsg)
          (send screenprinter (fn [_] (print-proof-step (build/apply-sub-to-term (:destination u) (:subst dermsg))
                                                        #{}
                                                        node
                                                        "conjunction-elimination"))))))
    (zipmap uch (repeat (count uch) dermsg))))

(defn conjunction-introduction
  "We are in an unasserted 'and' node, and would like to know if we now
   can say it is true based on message."
  [message node new-msgs]
  (let [new-ruis new-msgs
        der-rui-t (some #(when (= (:pos %) (count (@u-channels node))) %) new-ruis)
        der-rui-f (some #(when (pos? (:neg %)) %)new-ruis)
        dermsg-t (derivative-message (imessage-from-ymessage message node)
                                     :support-set (der-tag (:support-set der-rui-t)))
        dermsg-f (derivative-message message 
                                   :origin node
                                   :support-set (der-tag (:support-set der-rui-f))
                                   :type 'I-INFER
                                   :true? false)
        ich (@i-channels node)]
    (cond
      der-rui-t (do 
                  (when showproofs
                    (send screenprinter (fn [_] (print-proof-step node 
                                                                  (:support-set der-rui-t)
                                                                  "conjunction-introduction"))))
                  [true (der-tag (:support-set der-rui-t)) (zipmap ich (repeat (count ich) dermsg-t))])
      der-rui-f (do
                  (when showproofs
                    (send screenprinter (fn [_] (println "I derived: ~" node " by conjunction-introduction"))))
                  [false (der-tag (:support-set der-rui-f)) (zipmap ich (repeat (count ich) dermsg-f))])
      :else nil)))

(defn andor-elimination
  "Since the andor is true, we may have enough information to do elimination
   on it. "
  [message node new-msgs]
  (let [new-ruis new-msgs
        totparam (totparam node)
        pos-matches (filter #(= (:pos %) (:max node)) new-ruis)
        neg-matches (filter #(= (- totparam (:neg %)) (:min node)) new-ruis)]
    (when debug (send screenprinter (fn [_]  (println "NRUI" new-ruis))))
    
    ;(send screenprinter (fn [_]  (println "Pos" pos-matches) (println "Neg" neg-matches)))
    
    (merge 
      (when (seq pos-matches)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                    :origin node 
                                                                    :type 'U-INFER 
                                                                    :true? false 
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
                                                                           :Proposition {})
                                                              (:support-set pos-match)
                                                              node
                                                              (str (or 
                                                                     (build/syntype-fsym-map (syntactic-type-of node))
                                                                     "andor")
                                                                   "-elimination")))))))

        (add-matched-and-sent-messages (@msgs node) (set pos-matches) {:u-channel (set (vals der-msgs))})
                
        (into {} (for [[pos-match der-msg] der-msgs
                       u (@u-channels node)
                       :when (and (nil? ((:flaggedns pos-match) (:destination u)))
                                  (not (negated? (:destination u))))]
                   [u der-msg]))))
      
      (when (seq neg-matches)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                    :origin node 
                                                                    :type 'U-INFER 
                                                                    :true? true
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
                                                                           :Proposition {})
                                                              (:support-set neg-match)
                                                              node
                                                              (str (or 
                                                                     (build/syntype-fsym-map (syntactic-type-of node))
                                                                     "andor")
                                                                   "-elimination")))))))

        (add-matched-and-sent-messages (@msgs node) (set neg-matches) {:u-channel (set (vals der-msgs))})
                
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
  (let [new-ruis new-msgs
        totparam (totparam node)
        case1 (some #(when (or (> (:pos %) (:max node))
                               (> (:neg %) (- totparam (:min node)))) %) new-ruis)
        case2 (some #(when (and (>= (:pos %) (:min node))
                                (>= (:neg %) (- totparam (:max node)))) %) new-ruis)
        ich (@i-channels node)]
    (cond
      (isa? (syntactic-type-of node) :csneps.core/Andor)
      (when case2
        (let [dermsg (derivative-message message 
                                         :origin node
                                         :support-set (der-tag (:support-set case2))
                                         :type 'I-INFER
                                         :true? true)]
          (when showproofs 
            (send screenprinter (fn [_] (print-proof-step node
                                                          (der-tag (:support-set case2))
                                                          (str (or 
                                                                 (build/syntype-fsym-map (syntactic-type-of node))
                                                                 "param2op")
                                                               "-introduction")))))
          [true (der-tag (:support-set case2)) (zipmap ich (repeat (count ich) dermsg))]))
      (isa? (syntactic-type-of node) :csneps.core/Thresh)
      (when case1
        (let [dermsg (derivative-message message 
                                         :origin node
                                         :support-set (der-tag (:support-set case1))
                                         :type 'I-INFER
                                         :true? true)]
          (when showproofs 
            (send screenprinter (fn [_] (print-proof-step node 
                                                          (der-tag (:support-set case1))
                                                          (str (or 
                                                                 (build/syntype-fsym-map (syntactic-type-of node))
                                                                 "param2op")
                                                               "-introduction")))))
          [true (der-tag (:support-set case1)) (zipmap ich (repeat (count ich) dermsg))])))))
  
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
                                                                   :true? true 
                                                                   :support-set (os-union (:support-set %) (@support node))
                                                                   :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                                                   :taskid (:taskid message)))
                                     more-than-min-true-match))]
        
          (when showproofs 
            (doseq [[more-than-min-true-match der-msg] der-msgs
                    u (@u-channels node)
                    :when (and (not ((:flaggedns more-than-min-true-match) (:destination u)))
                               (build/pass-message? u der-msg))]
              (when (and (not ((:flaggedns more-than-min-true-match) (:destination u)))
                         (build/pass-message? u der-msg))
                  (send screenprinter (fn [_] (print-proof-step (build/apply-sub-to-term (:destination u) (:subst more-than-min-true-match)) 
                                                              (:support-set more-than-min-true-match)
                                                              node
                                                              (str (or 
                                                                     (build/syntype-fsym-map (syntactic-type-of node))
                                                                     "thresh")                                                               
                                                                   "-elimination")))))))
          
          (add-matched-and-sent-messages (@msgs node) (set more-than-min-true-match) {:u-channel (set (vals der-msgs))})
          
          (into {} (for [[more-than-min-true-match der-msg] der-msgs
                         u (@u-channels node)
                         :when (not ((:flaggedns more-than-min-true-match) (:destination u)))]
                     [u der-msg]))))
      
      (when (seq less-than-max-true-match)
        (let [der-msgs (into {} (map #(vector % (derivative-message %
                                                                   :origin node 
                                                                   :type 'U-INFER 
                                                                   :true? true 
                                                                   :support-set (os-union (:support-set %) (@support node))
                                                                   :fwd-infer? (when (or (:fwd-infer? message) (seq (@future-fw-infer node))) true)
                                                                   :taskid (:taskid message)))
                                     less-than-max-true-match))]
        
          (when showproofs 
            (doseq [[less-than-max-true-match der-msg] der-msgs
                    u (@u-channels node)
                    :when (and (not ((:flaggedns less-than-max-true-match) (:destination u)))
                               (build/pass-message? u der-msg))]
              (when (and (not ((:flaggedns less-than-max-true-match) (:destination u)))
                         (build/pass-message? u der-msg))
                  (send screenprinter (fn [_] (print-proof-step (build/apply-sub-to-term (:destination u) (:subst less-than-max-true-match)) 
                                                              (:support-set less-than-max-true-match)
                                                              node
                                                              (str (or 
                                                                     (build/syntype-fsym-map (syntactic-type-of node))
                                                                     "thresh")                                                               
                                                                   "-elimination")))))))
          
          (add-matched-and-sent-messages (@msgs node) (set less-than-max-true-match) {:u-channel (set (vals der-msgs))})
          
          (into {} (for [[less-than-max-true-match der-msg] der-msgs
                         u (@u-channels node)
                         :when (not ((:flaggedns less-than-max-true-match) (:destination u)))]
                     [u der-msg])))))))

(defn whquestion-infer 
  [message node]
  (when debug (send screenprinter (fn [_]  (println "Deriving answer:" (:subst message)))))
  ;; We don't need RUIs - the received substitutions must be complete since they
  ;; passed unification! Instead, lets use cached-terms.
  (dosync (alter instances assoc node (assoc (@instances node) (build/apply-sub-to-term node (:subst message) true) (:subst message)))))
  
  
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
          new-msgs (map #(derivative-message % 
                                             :origin node 
                                             :type 'I-INFER 
                                             :taskid (:taskid message)
                                             :support-set (os-union (:support-set %) (@support node))) 
                        inst-msgs) ;; using fwd-infer here is a bit of a hack.
          ich (@i-channels node)]
      ;(when showproofs
    
      (add-matched-and-sent-messages (@msgs node) (set inst-msgs) {:i-channel (set new-msgs)})
    
      (when (seq new-msgs)
        (send screenprinter (fn [_] (println "Policy " node " satisfied." inst-msgs))))
      (apply concat 
             (for [nmsg new-msgs] 
               (zipmap ich (repeat (count ich) nmsg)))))))

(defn g-chan-to-node?
  [orig dest]
  (some #(= (:destination %) dest) (@g-channels orig)))

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
              der-msg (derivative-message rcm
                                         :origin node
                                         :support-set inst-support
                                         :true? true
                                         :type 'I-INFER
                                         :fwd-infer? (:fwd-infer? message)
                                         :taskid (:taskid message))]
          
          ;(when instance 
            ;(println instance (os-union (:support-set rcm) (@support instance)))
            ;(dosync 
              ;(alter support assoc instance (os-concat (@support instance) inst-support))
             ; (alter instances assoc node (assoc (@instances node) instance (:subst rcm)))))
          
          (add-matched-and-sent-messages (@msgs node) #{rcm} {:i-channel #{der-msg} :g-channel #{der-msg}})
          
          (when (and showproofs instance 
                     (ct/asserted? node (ct/currentContext)) 
                     (filter #(build/pass-message? % der-msg) (union (@i-channels node) (@g-channels node))))
            (send screenprinter (fn [_] (print-proof-step instance
                                                          (:support-set rcm)
                                                          node
                                                          "generic-instantiation (1)"))))
          (doseq [ch (union (@i-channels node) (@g-channels node))]
            (if (and instance (genericAnalyticTerm? instance))
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
              outgoing-support (if (seq subst-support)
                                 (os-union (:support-set message)
                                           #{['der subst-support]})
                                 (:support-set message))
              der-msg (derivative-message message
                                          :origin node
                                          :support-set outgoing-support
                                          :type 'I-INFER
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
              
    
(defn arbitrary-instantiation
  [message node]
  (when debug (send screenprinter (fn [_]  (println "Arbitrary derivation:" (:subst message) "at" node))))
  (when (seq (:subst message)) ;; Lets ignore empty substitutions for now.
    (let [new-ruis (get-new-messages (@msgs node) message)
          resct (count (@restriction-set node))
          der-msg-t (filter #(= (:pos %) resct) new-ruis)
          new-msgs (map #(derivative-message % :origin node :type 'I-INFER :taskid (:taskid message) :fwd-infer? (:fwd-infer? message)) der-msg-t)
          gch (@g-channels node)]
      (when debug (send screenprinter (fn [_]  (println "NEWRUIS:" new-ruis))))
      (when (seq der-msg-t)
;        (send screenprinter (fn [_]  (println "NEWMESSAGE:" (count (for [msg new-msgs
;                                                                         ch gch]
;                                                                     [ch msg]))
;                                              new-msgs)))

        (add-matched-and-sent-messages (@msgs node) (set der-msg-t) {:g-channel (set new-msgs)})

        (when debug (send screenprinter (fn [_]  (println "NEWMESSAGE:" new-msgs))))
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
                           :subst (assoc 
                                    (:subst message) 
                                    node 
                                    (instantiate-indefinite node (:subst message)))) der-rui-t)
          gch (@g-channels node)]
      (when (seq der-rui-t)
        (when debug (send screenprinter (fn [_]  (println "NEWMESSAGE:" new-msgs))))
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
  (when debug (send screenprinter (fn [_]  (println "INFER: (elim) Inferring in:" node))))
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
  (when debug (send screenprinter (fn [_]  (println "INFER: (intro) Inferring in:" node))))
  (case (type-of node)
    :csneps.core/Conjunction (conjunction-introduction message node new-msgs)
    (:csneps.core/Andor 
     :csneps.core/Disjunction 
     :csneps.core/Xor
     :csneps.core/Nand
     :csneps.core/Thresh
     :csneps.core/Equivalence)  (param2op-introduction message node new-msgs)
    nil))

(defn initiate-node-task
  [term message]
  (when debug (send screenprinter (fn [_]  (println "INFER: Begin node task on message: " message "at" term))))
  ;(send screenprinter (fn [_]  (println "INFER: Begin node task on message: " message "at" term)))
  
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
    (let [result-term (if (:true? message)
                        (build/apply-sub-to-term term (:subst message))
                        (build/build (list 'not (build/apply-sub-to-term term (:subst message))) :Proposition {}))]
      
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
    
    (when (:true? message)
      ;; Elimination after a U-INFER message is different. 
      ;; Need to look at already matched messages, and combine with the new support, and relay that. 
      ;; Since we reason in all contexts, it won't result in any new derivations, just new reasons for old ones.
      ;; Each of the elimination rules should have a condition for U-INFER messages to do this.
      (when-let [result (elimination-infer message term (get-matched-messages (@msgs term)))]
        (when debug (send screenprinter (fn [_]  (println "INFER: Result Inferred " result))))
        (doseq [[ch msg] result] 
          (submit-to-channel ch msg)))))
  
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
      ;; AnalyticGeneric terms need to just forward the messages
      ;; on towards the variable term.
      (genericAnalyticTerm? term)
      (let [imsg (derivative-message message :origin term)]
        (when debug (send screenprinter (fn [_]  (println "INFER: AnalyticGeneric" term "forwarding message."))))
        (doseq [cqch (@i-channels term)] 
          (submit-to-channel cqch imsg)))
      ;; "Introduction" of a WhQuestion is really just collecting answers.
      (= (semantic-type-of term) :WhQuestion)
      (whquestion-infer message term)
      ;; Generic inference.
      (and 
        (genericTerm? term)
        (seq (:subst message)))
      (generic-infer message term)
      ;; Specific instance building.
      (and 
        (genericTerm? (:origin message))
        (build/specificInstanceOfGeneric? (:origin message) term (:subst message)))
      (do 
        (when (:true? message)
          (dosync (alter-support term (os-concat (@support term) (:support-set message)))))
        ;; Send this new info onward
        (let [imsg (derivative-message message
                                       :origin term
                                       :support-set (:support-set message)
                                       :type 'I-INFER)]
          ;; Save the message for future terms which might have channels
          ;; from this. Sometimes necessary for forward focused reasoning.
          (when (@msgs term) (add-matched-and-sent-messages (@msgs term) #{(sanitize-message message)} {:i-channel #{imsg}}))
          ;; Do the sending.
          (doseq [cqch (@i-channels term)] 
            (when (not= (:destination cqch) (:orign message)) ;; Don't just send it back where it came from.
              (submit-to-channel cqch imsg)))))
      ;; Arbs
      (arbitraryTerm? term)
      (if-let [[true? result] (arbitrary-instantiation message term)]
        (when result 
          (doseq [[ch msg] result] 
            (submit-to-channel ch msg)))
        ;; When introduction fails, try backward-in-forward reasoning. 
        (when (:fwd-infer? message)
          (backward-infer term #{term} nil)))
      ;; Otherwise...
      :else
      (let [new-msgs (get-new-messages (@msgs term) message)]
        ;; Try elimination
        (when-let [result (elimination-infer message term new-msgs)]
          (when debug (send screenprinter (fn [_]  (println "INFER: Result Inferred " result))))
          (doseq [[ch msg] result] 
            (submit-to-channel ch msg)))
        ;; Then try introduction
        (if-let [[true? spt result] (introduction-infer message term new-msgs)]
          (when result 
            (when debug (send screenprinter (fn [_]  (println "INFER: Result Inferred " result "," spt "," true?))))
            ;; Update support.
            (let [resterm (if true?
                            term
                            (build/variable-parse-and-build (list 'not term) :Propositional))]
              (dosync (alter-support resterm (os-concat (@support resterm) spt)))
              (when print-intermediate-results (println "> " resterm)))
            ;; Send results.
            (doseq [[ch msg] result] 
              (submit-to-channel ch msg)))
          ;; When introduction fails, try backward-in-forward reasoning. 
          (when (:fwd-infer? message)
            (backward-infer term #{term} nil))))))

  (when (@infer-status (:taskid message))
    (.decrement ^CountingLatch (@infer-status (:taskid message)))))
          
  
;;; Message Handling ;;;

(defn create-message-structure
  "Create the RUI structure for a rule node. For now,
   we always create an empty set. In the future, we'll create P-Trees
   and S-Indexes as necessary."
  [syntype dcs & {:keys [n]}]
  (cond
    (empty? (filter #(arbitraryTerm? %) (build/flatten-term dcs)))
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
    (into {} (filter #(ct/asserted? (first %) context) (@instances ques)))))

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
  
;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn- print-valve [ch]
  (let [selectors-this-ct (filter #(clojure.set/subset? @(:hyps (ct/find-context (second %))) @(:hyps (ct/currentContext))) @(:valve-selectors ch))]
    (cond
      @(:valve-open ch) "-"
      (seq selectors-this-ct) (str "~" (print-str (map #(first %) selectors-this-ct)) "~")
      :else "/")))

(defn ig-status []
  (doseq [x @csneps.core/TERMS]
    (doseq [i (@i-channels (second x))]
      (println (:originator i) "-I-" (count @(:waiting-msgs i)) (print-valve i) "->" (:destination i)))
    (doseq [u (@u-channels (second x))]
      (println (:originator u) "-U-" (count @(:waiting-msgs u)) (print-valve u) "->" (:destination u)))
    (doseq [g (@g-channels (second x))]
      (println (:originator g) "-G-" (count @(:waiting-msgs g)) (print-valve g) "->" (:destination g)))))

(defn print-all-waiting-msgs []
  (doseq [t (vals @TERMS)
          i (@ant-in-channels t)]
    (println @(:waiting-msgs i))))

