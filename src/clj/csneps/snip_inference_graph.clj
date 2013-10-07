(in-ns 'csneps.snip)

(load "snip_rui")
(load "snip_message")
(load "snip_linear_rui_set")
(load "snip_sindex")

;; Tracking inference tasks
(def ^:dynamic taskid 0)

;; For debug:
(def screenprinter (agent nil))
(def print-intermediate-results false)
(def print-results-on-infer false)
(def debug false)
(def showproofs true)

;; Asynchronous asserter
(def asserter (agent nil))
(def async-assert false)

;; Incremented whenever a message is submitted, and decremented once inference
;; on a message is complete, this variable allows us to determine if inference
;; is in progress, and attach a watch function for the purpose of notifying the
;; user, or for benchmarking.
(def to-infer (agent 0))

(declare initiate-node-task create-rui-structure get-rule-use-info open-valve)

(defn priority-partial
  [priority f & more] 
  (with-meta (apply partial f more) {:priority priority}))

;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; Concurrency control ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (def to-infer (agent 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Channel Maniupulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn submit-to-channel
  [^csneps.core.build.Channel channel ^csneps.snip.Message message]
  (when debug (send screenprinter (fn [_]  (println "MSGTX: " message))))
  
  ;; Filter
  (when ((:filter-fn channel) @(:substitution message))
	  ;; Switch
	  (when debug (send screenprinter (fn [_]  (println "SWITCH!: " ((:switch-fn channel) @(:substitution message))))))
	  (dosync (ref-set (:substitution message) ((:switch-fn channel) @(:substitution message))))
	  
	  (when (:fwd-infer? message)
	    (open-valve channel))
	  (if (build/valve-open? channel)
	    ;; Process as usual.
	    (do 
	      (send to-infer inc)
	      (when debug (send screenprinter (fn [_]  (println "MSGRX: " message))))
	      (.execute ^ThreadPoolExecutor executorService (priority-partial 1 initiate-node-task (:destination channel) message))
	      nil)
	    ;; Cache in the waiting-msgs
	    (dosync (alter (:waiting-msgs channel) conj message)))))

(defn submit-assertion-to-channels
  [term & {:keys [fwd-infer] :or {:fwd-infer false}}]
  ;; Send the information about this assertion forward in the graph.
  ;; Positive instances:
  (let [msg (new-message {:origin term, :support-set #{term}, :type 'I-INFER, :fwd-infer? fwd-infer})]
    (doall (map #(submit-to-channel % msg) @(:i-channels term))))
  ;; Conjunctions:
  ;; Conjunctions have no antecedents if they are true, so the Y-INFER messages must be passed on here
  (when (= (csneps/type-of term) :csneps.core/Conjunction)
    (let [msg (new-message {:origin term, :support-set #{term}, :type 'Y-INFER, :fwd-infer? fwd-infer})]
      (doall (map #(submit-to-channel % msg) @(:y-channels term)))))
  ;; Negations:
  ;; When the assertion makes terms negated, the negated terms need to send out
  ;; on their i-channels that they are now negated.
  (let [msg (new-message {:origin term, :support-set #{term}, :type 'I-INFER, :true? false, :fwd-infer? fwd-infer})
        dcs-map (cf/dcsRelationTermsetMap term)
        nor-dcs (when dcs-map (dcs-map (slot/find-slot 'nor)))]
    (when nor-dcs
      (doseq [negterm nor-dcs]
        (doall (map #(submit-to-channel 
                       % 
                       (new-message {:origin negterm, :support-set #{negterm}, :type 'I-INFER, :true? false, :fwd-infer? fwd-infer})) 
                    @(:i-channels negterm)))))))

(defn open-valve 
  [channel]
  ;; Start by opening the channel. Anything new should go right to the executor pool.
  (dosync (ref-set (:valve-open channel) true))
  ;; Add the waiting messages to the executor pool.
  (doseq [msg @(:waiting-msgs channel)]
    (send to-infer inc)
    (.execute ^ThreadPoolExecutor executorService (priority-partial 1 initiate-node-task (:destination channel) msg)))
  ;; Clear the waiting messages list.
  (dosync (alter (:waiting-msgs channel) empty)))

(defn close-valve
  [channel]
  (dosync (ref-set (:valve-open channel) false)))

;;;;;;;;;;;;;;;;;
;;; Inference ;;;
;;;;;;;;;;;;;;;;;

(defn negated?
  [term]
  (ct/asserted? (build/build (list 'not term) :Proposition {}) (ct/currentContext)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Control ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn backward-infer
  "Spawns tasks recursively to open valves in channels and begin performing
   backward-chaining inference. The network tries to derive term, and uses a
   set to track which nodes it has already visited."
  ([term] (backward-infer term -10 #{}))
  ([term depth visited] 
    (let [msg (new-message {:type 'BACKWARD-INFER, :priority depth})]
      (doseq [ch @(:ant-in-channels term)]
        (when-not (visited term)
          (send to-infer inc)
          (.execute ^ThreadPoolExecutor executorService (priority-partial -10 backward-infer ch msg depth (conj visited term)))))))
  ([channel message depth visited]
    (when (= (:type message) 'BACKWARD-INFER)
      (when-not (build/valve-open? channel)
        (when debug (send screenprinter (fn [_]  (println "BWMSG: Backward Infer " depth " - opening valve on channel" channel))))
        (open-valve channel))
      (backward-infer (:originator channel) (dec (:priority message)) visited))
    (send to-infer dec)))

(defn cancel-infer
  "Same idea as backward-infer, except it closes valves. Cancelling inference
   has top priority."
  ([term] 
    ;(when (> cpus-to-use 1)
      (let [msg (new-message {:type 'CANCEL-INFER})]
        (doseq [ch @(:ant-in-channels term)]
          (.execute ^ThreadPoolExecutor executorService (priority-partial Integer/MAX_VALUE cancel-infer ch msg)))));)
  ([channel message]
    (when (= (:type message) 'CANCEL-INFER)
      (when (build/valve-open? channel)
        (when debug (send screenprinter (fn [_]  (println "CANCEL: Cancel Infer - closing valve on channel" channel))))
        (close-valve channel)
        (cancel-infer (:originator channel))))))

(defn forward-infer
  "Begins inference in term. Ignores the state of valves in sending I-INFER and Y-INFER messages
   through the graph."
  [term]
  ;; We need to pretend that a Y-INFER message came in to this node.
  (send to-infer inc)
  (.execute ^ThreadPoolExecutor executorService (priority-partial 1 initiate-node-task term 
                                              (new-message {:origin nil, :support-set #{}, :type 'Y-INFER, :fwd-infer? true}))))

(defn unassert
  "Move forward through the graph recursively unasserting terms which depend on this one."
  ([term]
    (build/unassert term)
    (doseq [ch @(:i-channels term)]
      (.execute ^ThreadPoolExecutor executorService (priority-partial Integer/MAX_VALUE unassert (:destination ch) (new-message {:origin term :type 'UNASSERT}) false)))
    (doseq [ch @(:y-channels term)]
      (.execute ^ThreadPoolExecutor executorService (priority-partial Integer/MAX_VALUE unassert (:destination ch) (new-message {:origin term :type 'UNASSERT}) true))))
  ([node message ych?]
    (when debug (send screenprinter (fn [_]  (println "Unassert: " message "at" node))))
    (cond 
      (and (not ych?)
           (= (type node) csneps.core.Implication))
      (doseq [ch @(:y-channels node)]
        (.execute ^ThreadPoolExecutor executorService (priority-partial Integer/MAX_VALUE unassert (:destination ch) (new-message {:origin (:origin message) :type 'UNASSERT}) true)))
      (let [oscont (map #(contains? % (:origin message)) @(:support node))]
        (and ych?
             (not (empty? oscont))
             (every? true? oscont)))
      (unassert node))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Inference Rules ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn negation-elimination
  "Invert the truth of the :true? key in the message, and pass onward."
  [message node]
  (let [new-ruis (get-rule-use-info (:ruis node) (rui-from-message message))
        dermsg (derivative-message message
                                   :origin node
                                   :support-set (conj (:support-set message) node)
                                   :true? false
                                   :type 'Y-INFER)
        ych @(:y-channels node)]
    (when debug (send screenprinter (fn [_]  (println "NEX" new-ruis))))
    nil
    (when-not (empty? new-ruis)
      (zipmap ych (repeat (count ych) dermsg)))
    ))

(defn negation-introduction
  "Pretty much conjunction-introduction, but with :neg instead of :pos"
  [message node]
  (let [new-ruis (get-rule-use-info (:ruis node) (rui-from-message message))
        der-rui (some #(= (:neg %) (count @(:y-channels node))) new-ruis)
        dermsg (derivative-message message
                                   :origin node
                                   :support-set (conj (:support-set message) node)
                                   :true? true
                                   :type 'I-INFER)
        ich @(:i-channels node)]
    (when debug (send screenprinter (fn [_]  (println "N-Int" new-ruis "\n" der-rui))))
    (when der-rui
      (dosync (alter (:support node) conj (:support-set message)))
      [true (zipmap ich (repeat (count ich) dermsg))])))
  

(defn numericalentailment-elimination
  "Since the implication is true, send a Y-INFER message to each
   of the consequents." 
  [message node]
  ;; If the node only requires 1 antecedent to be true, any incoming positive
  ;; antecedent is enough to fire the rule. In these cases it isn't necessary to
  ;; maintain the RUI structure. 
  (let [msgrui (rui-from-message message)]
	  (if (> (:min node) 1)
	    (when (:true? message)
	      (cancel-infer node)
        (apply conj {} (doall (map #(vector % (RUI->message msgrui node 'Y-INFER true (:fwd-infer? message)))
                                   (filter #(not (ct/asserted? % (ct/currentContext))) @(:y-channels node)))))))
   
       (let [new-ruis (get-rule-use-info (:ruis node) msgrui)
             match-ruis (some #(when (>= (:pos %) (:min node))
                                 %)
                              new-ruis)]
         (when match-ruis 
           (cancel-infer node)
           (apply conj {} (doall (map #(vector % (RUI->message match-ruis node 'Y-INFER true (:fwd-infer? message)))
                                      (filter #(not (ct/asserted? % (ct/currentContext))) @(:y-channels node)))))))))

(defn numericalentailment-introduction
  ""
  [message node])

(defn conjunction-elimination
  "Since the and is true, send a Y-INFER message to each of the
   consequents."
  [message node]
  (let [dermsg (derivative-message message 
                                   :origin node
                                   :support-set (conj (:support-set message) node)
                                   :type 'Y-INFER)
        ych @(:y-channels node)]
    (when debug (send screenprinter (fn [_]  (println "ELIMINATING"))))
    (zipmap ych (repeat (count ych) dermsg))))

(defn conjunction-introduction
  "We are in an unasserted 'and' node, and would like to know if we now
   can say it is true based on message."
  [message node]
  (let [new-ruis (get-rule-use-info (:ruis node) (rui-from-message message))
        der-rui-t (some #(= (:pos %) (count @(:y-channels node))) new-ruis)
        der-rui-f (some #(> (:neg %) 0) new-ruis)
        dermsg-t (imessage-from-ymessage message node)
        dermsg-f (derivative-message message 
                                   :origin node
                                   :support-set (conj (:support-set message) node)
                                   :type 'I-INFER
                                   :true? false)
        ich @(:i-channels node)]
    (cond
      der-rui-t [true (zipmap ich (repeat (count ich) dermsg-t))]
      der-rui-f [false (zipmap ich (repeat (count ich) dermsg-f))]
      :else nil)))

(defn andor-elimination
  "Since the andor is true, we may have enough information to do elimination
   on it. "
  [message node]
  (let [new-ruis (get-rule-use-info (:ruis node) (rui-from-message message))
        totparam (csneps/totparam node)
        pos-match (some #(when (= (:pos %) (:max node))
                           %) 
                        new-ruis)
        neg-match (some #(when (= (- totparam (:neg %)) (:min node)) %) new-ruis)]
    (when debug (send screenprinter (fn [_]  (println "NRUI" new-ruis))))
    (or 
      (when pos-match
        (apply conj {} (doall (map #(when (and (not ((:flaggedns pos-match) (:destination %)))
                                               (not (negated? (:destination %))))
                                      [% (RUI->message pos-match node 'Y-INFER false (:fwd-infer? message))])
                                   @(:y-channels node)))))
      (when neg-match
        (apply conj {} (doall (map #(when (and (nil? ((:flaggedns neg-match) (:destination %)))
                                               (not (ct/asserted? (:destination %) (ct/currentContext))))
                                      [% (RUI->message neg-match node 'Y-INFER true (:fwd-infer? message))])
                                   @(:y-channels node))))))))

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
  [message node]
  (let [new-ruis (get-rule-use-info (:ruis node) (rui-from-message message))
        merged-rui (when new-ruis (reduce merge new-ruis)) ;; If they contradict, we have other problems...
        totparam (csneps/totparam node)
        case1 (and merged-rui
                   (> (:pos merged-rui) (:max node))
                   (> (:neg merged-rui) (- totparam (:min node))))
        case2 (and merged-rui
                   (>= (:pos merged-rui) (:min node))
                   (>= (:neg merged-rui) (- totparam (:max node))))
        dermsg-t (imessage-from-ymessage message node)
        dermsg-f (derivative-message message 
                      :origin node
                      :support-set (conj (:support-set message) node)
                      :type 'I-INFER
                      :true? false)
        ich @(:i-channels node)]
    (when debug (send screenprinter (fn [_]  (println case1 case2))))
    (cond
      (isa? (csneps/syntactic-type-of node) :csneps.core/Andor)
      (if case2
        [true (zipmap ich (repeat (count ich) dermsg-t))])
      (isa? (csneps/syntactic-type-of node) :csneps.core/Thresh)
      (if case1
        [true (zipmap ich (repeat (count ich) dermsg-t))]))))
  
(defn thresh-elimination
  "Thesh is true if less than min or more than max."
  [message node]
  (let [new-ruis (get-rule-use-info (:ruis node) (rui-from-message message))
        totparam (csneps/totparam node)
        ;; Case 1: There are >= minimum true. Therefore > maximum must be true. 
        ;; If there are totparam - max - 1 false, then we can make the rest true.
        more-than-min-true-match (some #(when (and (>= (:pos %) (:min node))
                                                   (= (:neg %) (- totparam (:max node) 1)))
                                          %)
                                       new-ruis)
        ;; Case 2: There are enough false cases such that maximum could not be true.
        ;; Therefore the minimum must be true. If enough of them are already, the rest
        ;; are false.
        less-than-max-true-match (some #(when (and (>= (:neg %) (- totparam (:max node)))
                                                   (= (:pos %) (- (:min node) 1)))
                                          %)
                                          new-ruis)]
    (or (when more-than-min-true-match
          (when showproofs 
            (doseq [y @(:y-channels node)]
              (when-not ((:flaggedns more-than-min-true-match) (:destination y))
                 (send screenprinter (fn [_] (println "Derived: " (:destination y) " by thresh-elimination"))))))
          (apply conj {} (doall (map #(when-not ((:flaggedns more-than-min-true-match) (:destination %))
                                        [% (RUI->message more-than-min-true-match node 'Y-INFER true (:fwd-infer? message))])
                                     @(:y-channels node)))))
        (when less-than-max-true-match
          (when showproofs 
            (doseq [y @(:y-channels node)]
              (when-not (when (nil? ((:flaggedns less-than-max-true-match) (:destination y)))
                 (send screenprinter (fn [_] (println "Derived: " (:destination y) " by thresh-elimination")))))))
          (apply conj {} (doall (map #(when (nil? ((:flaggedns less-than-max-true-match) (:destination %)))
                                        [% (RUI->message less-than-max-true-match node 'Y-INFER false (:fwd-infer? message))])
                                     @(:y-channels node))))))))

(defn whquestion-infer 
  [message node]
  (send screenprinter (fn [_]  (println "Deriving answer:" @(:substitution message))))
  ;; We don't need RUIs - the received substitutions must be complete since they
  ;; passed unification!
  )

(defn elimination-infer
  "Input is a message and node, output is a set of messages derived."
  [message node]
  (when debug (send screenprinter (fn [_]  (println "INFER: (elim) Inferring in:" node))))
  (case (csneps/type-of node)
    :csneps.core/Negation (negation-elimination message node)
    :csneps.core/Conjunction (conjunction-elimination message node)
    (:csneps.core/Numericalentailment
     :csneps.core/Implication) (numericalentailment-elimination message node)
    (:csneps.core/Andor 
     :csneps.core/Disjunction 
     :csneps.core/Xor
     :csneps.core/Nand)  (andor-elimination message node)
    (:csneps.core/Thresh
     :csneps.core/Equivalence) (thresh-elimination message node)
    nil ;default
    ))

(defn introduction-infer
  ""
  [message node]
  (when debug (send screenprinter (fn [_]  (println "INFER: (intro) Inferring in:" node))))
  (case (csneps/type-of node)
    :csneps.core/Negation (negation-introduction message node)
    :csneps.core/Conjunction (conjunction-introduction message node)
    (:csneps.core/Numericalentailment
     :csneps.core/Implication) (numericalentailment-introduction message node)
    (:csneps.core/Andor 
     :csneps.core/Disjunction 
     :csneps.core/Xor
     :csneps.core/Nand
     :csneps.core/Thresh
     :csneps.core/Equivalence)  (param2op-introduction message node)
    nil))

(defn derivative-i-message
  [message node]
  (if (= (:type message) 'FORWARD-INFER)
    (derive-forward-infer message node)
    (imessage-from-ymessage message node)))

(defn initiate-node-task
  [term message]
  (when debug (send screenprinter (fn [_]  (println "INFER: Begin node task on message: " message "at" term))))
  ;; ---- Elimination Rules ---- ;;
  ;; If I'm already asserted, and I just received an I-INFER message,
  ;; I should attempt to eliminate myself.
  (when (and (ct/asserted? term (ct/currentContext))
             (= (:type message) 'I-INFER))
    (when-let [result (elimination-infer message term)]
      (when debug (send screenprinter (fn [_]  (println "INFER: Result Inferred " result))))
      (doseq [[ch msg] result] 
        (submit-to-channel ch msg))))
  
  ;; If I have just received a Y-INFER message, I must make myself
  ;; either true or false according to the message, report that
  ;; new belief, and attempt elimination.
  (when (or (= (:type message) 'Y-INFER)
            ;(backward-infer term) ;; Sure this goes here?
            )
    (if (:true? message) 
      ;; Step 2a: Since I am supposed to be asserted, if I'm not,
      ;; make the assertion and let my i-channels know I am now
      ;; true.
      (do
        (dosync (alter (:support term) conj (:support-set message)))
        (when-not (ct/asserted? term (ct/currentContext))
          (if print-intermediate-results
            (send screenprinter (fn [_]  (println "> " (build/assert-term term (ct/currentContext) :der))))
            (if async-assert 
              (send asserter (fn [_] (build/assert-term term (ct/currentContext) :der)))
              (build/assert-term term (ct/currentContext) :der)))
          (let [imsg (derivative-message message
                                   :origin term
                                   :support-set (conj (:support-set message) term)
                                   :true? true
                                   :type 'I-INFER)]
            (doseq [cqch @(:i-channels term)] (submit-to-channel cqch imsg))))
        ;; Step 3: Apply elimination rules and report results
        (when-let [result (elimination-infer message term)]
          (when debug (send screenprinter (fn [_]  (println "INFER: Result Inferred " result))))
          (doseq [[ch msg] result] 
            (submit-to-channel ch msg))))
      (do
        (let [neg-term (build/build (list 'not term) :Entity {})]
          (dosync (alter (:support neg-term) conj (:support-set message)))
          (when-not (ct/asserted? neg-term (ct/currentContext))
            (if print-intermediate-results
              (send screenprinter (fn [_]  (println "> " (build/assert-term neg-term (ct/currentContext) :der))))
              (build/assert-term neg-term (ct/currentContext) :der))
            (let [imsg (derivative-message message
                                           :origin term
                                           :support-set (conj (:support-set message) term)
                                           :true? false
                                           :type 'I-INFER)]
              (doseq [cqch @(:i-channels term)] (submit-to-channel cqch imsg)))
            )))))
  ;; ---- Introduction Rules ---- ;;
  (cond 
    ;; "Introduction" of a WhQuestion is really just collecting answers.
    (and
      (= (:type message) 'I-INFER)
      (= (csneps/semantic-type-of term) :WhQuestion))
    (whquestion-infer message term)
    ;; Normal introduction for derivation.
    (and 
      (not (ct/asserted? term (ct/currentContext)))
      (= (:type message) 'I-INFER))
    (when-let [[true? result] (introduction-infer message term)]
      (send screenprinter (fn [_]  (println "INFER: Result Inferred " result "," true?)))
      (when result 
        (if true?
          (if print-intermediate-results
            (send screenprinter (fn [_]  (println "> " (build/assert term (ct/currentContext) :der))))
            (build/assert term (ct/currentContext) :der))
          (if print-intermediate-results
            (send screenprinter (fn [_] (println "> " (build/assert (list 'not term) (ct/currentContext) :der))))
            (build/assert (list 'not term) (ct/currentContext) :der)))
        (doseq [[ch msg] result] 
          (submit-to-channel ch msg)))))
  (send to-infer dec))
          
  
;;; RUI Handling ;;;

(defn create-rui-structure
  "Create the RUI structure for a rule node. For now,
   we always create an empty set. In the future, we'll create P-Trees
   and S-Indexes as necessary."
  [syntype dcs]
  (make-linear-rui-set))

(build/fix-fn-defs submit-to-channel new-message create-rui-structure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User-oriented functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn backward-infer-derivable [term context]
  (binding [taskid (rand-int java.lang.Integer/MAX_VALUE)]
    (let [term (build/build term :Proposition #{})
          answers (ref #{})
          update-answers (fn [ref key oldvalue newvalue]
                           (when (and (= newvalue 0) (not= oldvalue 0))
                             (if (ct/asserted? term context)
                               (dosync (alter answers conj term))
                               (dosync (ref-set answers nil)))))]
      (add-watch to-infer :to-infer update-answers)
      (if (not (empty? @(:ant-in-channels term)))
        (do 
          (backward-infer term)
          @(future (while (and (empty? @answers) (not (nil? @answers)))
                     (Thread/sleep 10))
             (remove-watch to-infer :to-infer)
             @answers))
        #{}))))
   
(defn backward-infer-answer
  "Used for answering WhQuestions"
  [ques context]
  (binding [taskid (rand-int java.lang.Integer/MAX_VALUE)]
    (let [answers (ref #{})
          update-answers (fn [ref key oldvalue newvalue]
                           (when (and (= newvalue 0) (not= oldvalue 0))
                             (dosync (ref-set answers nil))))]
      (add-watch to-infer :to-infer update-answers)
      (if (not (empty? @(:ant-in-channels ques)))
        (do 
          (backward-infer ques)
          @(future (while (and (empty? @answers) (not (nil? @answers)))
                     (Thread/sleep 10))
             (remove-watch to-infer :to-infer)
             @answers))
        #{}))))

(defn cancel-infer-of [term])

;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn- print-valve [ch]
  (if @(:valve-open ch) "-" "/"))

(defn ig-status []
  (doseq [x @csneps.core/TERMS]
    (doseq [y @(:i-channels (second x))]
      (println (:originator y) "-I-" (count @(:waiting-msgs y)) (print-valve y) "->" (:destination y)))
    (doseq [y @(:y-channels (second x))]
      (println (:originator y) "-U-" (count @(:waiting-msgs y)) (print-valve y) "->" (:destination y)))))

