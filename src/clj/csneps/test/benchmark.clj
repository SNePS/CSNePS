(ns csneps.test.benchmark
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.printer :as print]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.snip :as snip]
            [clojure.set :as set]
            [csneps.core.snuser :as snuser]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Time and Statistics Variables + Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def runtime (atom 0))
(def iterations 10)
(def iterations-left (atom iterations))

(defn log-elapsed
  [start-time]
  (swap! runtime + (/ (- (. java.lang.System (clojure.core/nanoTime)) start-time) 1000000.0)))

(defn reset-benchmark
  []
  (def runtime (atom 0))
  (def iterations-left (atom iterations))
  ;(remove-watch csneps.snip/to-infer :to-infer)
  )

(defn print-time
  []
  (println "Run Time: " @runtime))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Entailments ;;;

;; General function to generate entailments automatically. entailsym can be things like
;; =v> or if. 
(defn generate-entailment
  [entailsym & {:keys [antset antcount cqset cqcount assert?]}]
  (let [ants (if antset antset (set (map gensym (repeat antcount "ant"))))
        cqs (if cqset cqset (set (map gensym (repeat cqcount "cq"))))]
    (if assert?
      [ants cqs (snuser/assert (list entailsym ants cqs))]
      [ants cqs (snuser/defineTerm (list entailsym ants cqs))])))

(defn generate-entailment-chain-cqroot
  [entailsym branching-factor maxdepth]
  (loop [depth 0
         cqset '#{cq}]
    (if (< depth maxdepth)
      (recur (inc depth)
             (apply clojure.set/union
                    (map first
                         (map #(generate-entailment entailsym :antcount branching-factor :cqset #{%} :assert? true) cqset))))
      (doall (map #(snuser/assert %) cqset)))))

(defn generate-entailment-chain-antroot
  [entailsym branching-factor maxdepth]
  (loop [depth 0
         antset '#{ant}]
    (if (< depth maxdepth)
      (recur (inc depth)
             (apply clojure.set/union
                    (map second 
                         (map #(generate-entailment entailsym :antset #{%} :cqcount branching-factor :assert? true) antset))))
      antset)))

;;; And ;;;

(defn generate-and
  [assert? ant-ct]
  (let [ant-syms (map gensym (repeat ant-ct "ant"))
        term (if assert?
               (do 
                 ;(doall (map #(snuser/assert %) ant-syms))
                 (snuser/assert (list* 'and ant-syms)))
               (snuser/defineTerm (list* 'and ant-syms)))]
    (if assert?
      ;; If the and is snuser/asserted, don't snuser/assert the
      ;; ants, but return them.
      (do 
        (snuser/assert (list* 'and ant-syms))
        ant-syms)
      ;; Otherwise, snuser/assert the ants, and return the rule
      (do 
        (doall (map #(snuser/assert %) ant-syms))
        (:name (snuser/defineTerm (list* 'and ant-syms)))))))

;;; Andor ;;;

(defn generate-andor
  [assert? ant-ct]
  (let [min-ct (int (inc (rand ant-ct)))
        max-ct (+ min-ct (int (rand (- ant-ct min-ct))))
        true-ants (map gensym (repeat max-ct "ant"))
        false-ants (map gensym (repeat (- ant-ct max-ct) "ant"))
        term (if assert?
               (snuser/assert (list* 'andor (list min-ct max-ct) (concat true-ants false-ants)))
               (snuser/defineTerm (list* 'andor (list min-ct max-ct) (concat true-ants false-ants))))]
    [true-ants false-ants term]))

;;;;;;;;;;;;;;;;;;;;;;
;;; Graph Builders ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn write-entailment-graph-cqroot
  [entailsym bf depth outfile]
  (snuser/clearkb true)
  (generate-entailment-chain-cqroot entailsym bf depth)
  (print/writeKBToTextFile outfile))
    
(defn write-entailment-graph-antroot
  [entailsym bf depth outfile]
  (snuser/clearkb true)
  (generate-entailment-chain-antroot entailsym bf depth)
  (print/writeKBToTextFile outfile))


;;;;;;;;;;;;;;;;;;
;;; Benchmarks ;;;
;;;;;;;;;;;;;;;;;;

(defn benchmark-fwd-1
  [buildgraphfn]
  (while (>= (swap! iterations-left dec) 0)
    (snuser/clearkb true)
    (buildgraphfn)
    (let [start-time (. java.lang.System (clojure.core/nanoTime))]
      (snuser/assert! 'ant)
      (log-elapsed start-time)
      (print-time))))

(defn benchmark-fwd
  [entailsym bf depth]
  (reset-benchmark)
  (let [buildgraphfn #(generate-entailment-chain-antroot entailsym bf depth)]
    (benchmark-fwd-1 buildgraphfn)))

(defn benchmark-fwd-file
  [fname itrs include-build-time?]
  (def iterations itrs)
  (reset-benchmark)
  (while (>= (swap! iterations-left dec) 0)
    (snuser/clearkb true)
    (if include-build-time?
      (let [start-time (. java.lang.System (clojure.core/nanoTime))]
        (load-file fname)
        (snuser/assert! 'ant)
        (log-elapsed start-time)
        (print-time))
      (do
        (load-file fname)
        (let [start-time (. java.lang.System (clojure.core/nanoTime))]
          (snuser/assert! 'ant)
          (log-elapsed start-time)
          (print-time))))))


(defn benchmark-bwd-1
  [buildgraphfn]
  (while (>= (swap! iterations-left dec) 0)
    (snuser/clearkb true)
    (buildgraphfn)
    (let [start-time (. java.lang.System (clojure.core/nanoTime))]
      (snip/backward-infer (snuser/find-term 'cq))
      (log-elapsed start-time)
      (print-time))))

(defn benchmark-bwd
  [entailsym bf depth itrs]
  (def iterations itrs)
  (reset-benchmark)
  (let [buildgraphfn #(generate-entailment-chain-cqroot entailsym bf depth)]
    (benchmark-bwd-1 buildgraphfn)))

(defn benchmark-bwd-file
  [fname itrs include-build-time?]
  (def iterations itrs)
  (reset-benchmark)
  (while (>= (swap! iterations-left dec) 0)
    (snuser/clearkb true)
    (if include-build-time?
      (let [start-time (. java.lang.System (clojure.core/nanoTime))]
        (load-file fname)
        (snip/backward-infer (snuser/find-term 'cq))
        (log-elapsed start-time)
        (print-time))
      (do
        (load-file fname)
        (let [start-time (. java.lang.System (clojure.core/nanoTime))]
          (snip/backward-infer (snuser/find-term 'cq))
          (log-elapsed start-time)
          (print-time))))))
