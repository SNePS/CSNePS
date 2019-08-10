(ns csneps.snip.inference-graph.concurrent
  (:use [csneps.configuration])
  (:import [java.util Comparator]
           [java.util.concurrent TimeUnit LinkedBlockingQueue PriorityBlockingQueue ThreadPoolExecutor]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Concurrency Control for IG ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Fixed Thread Pool of size 2 * processors, using queue as it's queue.

(def executorService nil)

(defn startExecutor
  []
  (def executorService (ThreadPoolExecutor.
                         ig-cpus-to-use
                         ig-cpus-to-use
                         (Long/MAX_VALUE) TimeUnit/NANOSECONDS queue))
  (.prestartAllCoreThreads ^ThreadPoolExecutor executorService))

(defn resetExecutor
  []
  (.shutdownNow ^ThreadPoolExecutor executorService)
  (.awaitTermination ^ThreadPoolExecutor executorService 60 TimeUnit/SECONDS)
  (when-not (.isTerminated ^ThreadPoolExecutor executorService)
    (println "ThreadPoolExecutor did not terminate."))
  (.clear ^PriorityBlockingQueue queue)
  (def executorService (ThreadPoolExecutor.
                         ig-cpus-to-use
                         ig-cpus-to-use
                         (Long/MAX_VALUE) TimeUnit/NANOSECONDS queue))
  (.prestartAllCoreThreads ^ThreadPoolExecutor executorService)
  (def infer-status (ref {nil (edu.buffalo.csneps.util.CountingLatch.)})))

;; Only used when exiting.
(defn shutdownExecutor
  []
  (.shutdownNow ^ThreadPoolExecutor executorService)
  (.awaitTermination ^ThreadPoolExecutor executorService 60 TimeUnit/SECONDS)
  (when-not (.isTerminated ^ThreadPoolExecutor executorService)
    (println "ThreadPoolExecutor did not terminate.")))

;;; Experimental attempt at pausing inference.
(let [waiting-queue (LinkedBlockingQueue.)]
  (defn pause-execute
    []
    (.drainTo ^PriorityBlockingQueue queue waiting-queue))

  (defn resume-execute
    []
    (.drainTo ^LinkedBlockingQueue waiting-queue queue)))