(ns test
 (:import (java.util.concurrent Executors ExecutorCompletionService)))
;;;Testing ad-hoc class hierarchys:

;(defprotocol SYNTACTIC-TYPE
;  (type [t]))

(defrecord Term
  [name
   type])
;  SYNTACTIC-TYPE
;  (type [t] ::Term))

(defrecord Atom
  [name
   type])
 ; SYNTACTIC-TYPE
;  (type [t] ::Atom))

(def type-hierarchy (derive (make-hierarchy) ::Atom ::Term))

;(print-name t2)(derive ::Atom ::Term)

(defmulti print-name 
  (fn [t] [(:type t)]))

(defmethod print-name 
  [::Term] [t]
  (print t)
  (println (:name t)))

(def t (Term. "Test-Term-1" ::Term))
(def t2 (Atom. "Test-Atom-1" ::Atom))

;(defmethod print-method test.Term [o w]
;  (.write w (str "Hi")))


(defn testexcept
  []
  (let [test (try
    (throw (/ 1 1))
    (catch Exception e ())
    (finally "Sup"))]
  (println "Here!" test)))



;(def t1 (assert '(Isa x y)))
;(sneps3/build-upcsets t1)
;@(:up-cablesetw (get @sneps3/*TERMS* 'x))
;(find/findfrom (get @sneps3/*TERMS* 'x) (relations/find-slot 'member))


;(def t1 (snuser/assert '(Isa x y)))

(defn testrest
  [& clearall]
  (println clearall))


;;Testing dispatch on subtypes.

(defrecord Testrec [type])

(defmulti typepr
  (fn [t] [(:type t)]))

(defmethod typepr
  [::Molecular] [t]
  (print ::Molecular))

(defmethod typepr
  [::Disjunction] [t]
  (print ::Disjunction))

(derive ::Disjunction ::Molecular)

(def mol (Testrec. ::Molecular))
(def disj (Testrec. ::Disjunction))


;;; Concurrency Tests

(defn falseWait
  "Returns false after a random amount of time, up to 10sec"
  []
  (let [waittime (* (Math/random) 5000)]
    (println "False thread waiting " waittime "ms")
    (. Thread (sleep waittime)))
  false)

(defn trueWait
  "Returns true after a random amount of time, up to 10sec"
  []
  (let [waittime (* (Math/random) 5000)]
    (println "True thread waiting " waittime "ms")
    (. Thread (sleep waittime)))
  true)

(defn simulateResults
  "Simulates the results of inference by producing calls to falseWait and trueWait fr and tr
times, respectively."
  [fr tr]
  (shuffle (concat (replicate fr falseWait) (replicate tr trueWait))))

(let [ctp (Executors/newCachedThreadPool)]

  (defn ebor [fr tr]
    "Performs eager-beaver or based on fr false inputs and tr true inputs."
    (let [ecs (ExecutorCompletionService. ctp)
          tasks (simulateResults fr tr)
          results (map #(.submit ecs ^Callable (bound-fn* %)) tasks) ;;bound-fn* is only there to correct console output of printlns.
          ret (ref false)]
      ;; The ExecutiorCompletionService has a BlockingQueue attached to it which
      ;; is filled as the futures return results. We take them off as we receive
      ;; them and check their truth value, stopping when we have a true value.
      (doseq [n (range (count results))
              :while (not @ret)]
        (let [resultfuture (.take ecs)
              resultval (.get resultfuture)]
          (when resultval (dosync (ref-set ret true)))))
      ;; After we're done, cancel any extra futures we may have around.
      (doseq [r results]
        (future-cancel r))
      ;; Return the result.
      @ret))

  (defn eband [fr tr]
    "Performs eager-beaver or based on fr false inputs and tr true inputs."
    (let [ecs (ExecutorCompletionService. ctp)
          tasks (simulateResults fr tr)
          results (map #(.submit ecs ^Callable (bound-fn* %)) tasks) ;;bound-fn* is only there to correct console output of printlns.
          ret (ref true)]
      ;; The ExecutiorCompletionService has a BlockingQueue attached to it which
      ;; is filled as the futures return results. We take them off as we receive
      ;; them and check their truth value, stopping when we have a true value.
      (doseq [n (range (count results))
              :while @ret]
        (let [resultfuture (.take ecs)
              resultval (.get resultfuture)]
          (when (not resultval) (dosync (ref-set ret false)))))
      ;; After we're done, cancel any extra futures we may have around.
      (doseq [r results]
        (future-cancel r))
      ;; Return the result.
      @ret))


  (defn ebandor [fr tr min max]
    "Performs eager-beaver andor based on fr false inputs and tr true inputs."
    (let [ecs (ExecutorCompletionService. ctp)
          tasks (simulateResults fr tr)
          results (map #(.submit ecs ^Callable (bound-fn* %)) tasks) ;;bound-fn* is only there to correct console output of printlns.
          total (+ fr tr)
          posinst (ref 0)
          neginst (ref 0)
          ret (ref false)
          done (ref false)]
      ;; The ExecutiorCompletionService has a BlockingQueue attached to it which
      ;; is filled as the futures return results. We take them off as we receive
      ;; them and check their truth value, stopping when we have a true value.
      (doseq [n (range (count results))
              :while (not @done)]
        (let [resultfuture (.take ecs)
              resultval (.get resultfuture)]
          (println (+ n 1) " of " total "- result rec'd: " resultval)
          (if resultval
            (dosync (ref-set posinst (inc @posinst)))
            (dosync (ref-set neginst (inc @neginst))))
          (cond
            ;; If we have more true than the max allowed, we can stop with false.
            (> @posinst max)
            (dosync (ref-set done true))
            ;; If we have more false than the max allowed, we can stop with false.
            (> @neginst (- total min))
            (dosync (ref-set done true))
            ;; If we have more true than the minimim, but there's less than or = tp
            ;; max - posinst items left, we can stop and say it's true.
            (and (>= @posinst min)
              (<= (- total @posinst @neginst) (- max @posinst)))
            (dosync (ref-set ret true) (ref-set done true)))))
      ;; After we're done, cancel any extra futures we may have around.
      (doseq [r results]
        (future-cancel r))
      ;; Return the result.
      @ret))
  )


(defn testmap [ct]
  (map testmap (list (dec ct))))




;(defn expand-qmvar
;  [forms]
;  (map #(postwalk (fn [x] (if (re-matches #"^\?.*" (str x)) (list 'binds x) x)) %) forms))





