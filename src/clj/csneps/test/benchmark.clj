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

(defn generate-arb
  [rescount]
  (let [id (gensym "")
        varlabel (symbol (str "x" id))]
    (list* 'every varlabel
           (for [x (range rescount)
                 :let [resfsym (symbol (str "r" id "-" x))]]
             (list resfsym varlabel)))))

(defn generate-arb-instance
  [arbexpr]
  (let [inst (gensym "inst")
        restrictions (nnext arbexpr)]
    (doseq [r restrictions]
      (snuser/assert (list (first r) inst)))
    inst))

(defn generate-arb-instances
  [arbexpr instct]
  (for [n (range instct)]
    (generate-arb-instance arbexpr)))

(defn generate-term 
  [prefix args]
  (let [id (gensym "")]
    (if (= args nil)
      (symbol (str prefix id))
      (list* (symbol (str prefix id)) args))))

;;; Entailments ;;;

;; General function to generate entailments automatically. entailsym can be things like
;; =v> or if. 
(defn generate-entailment
  [entailsym & {:keys [antset antcount cqset cqcount assert? args]}]
  (let [ants (if antset antset (set (for [i (range antcount)]
                                      (generate-term "ant" args))))
        cqs (if cqset cqset (set (for [i (range antcount)]
                                   (generate-term "cq" args))))]
    (if assert?
      [ants cqs (snuser/assert (list entailsym ants cqs))]
      [ants cqs (snuser/defineTerm (list entailsym ants cqs))])))

(defn generate-entailment-chain-cqroot
  [entailsym branching-factor maxdepth arbcount rescount instcount]
  (let [args (when (> arbcount 0)
               (for [i (range arbcount)]
                 (generate-arb rescount)))
        instances (when args
                    (map #(generate-arb-instances % instcount) args))
        inst-invert (for [n (range (count (first instances)))]
                      (map #(nth % n) instances))]
    (println inst-invert)
    (loop [depth 0
           cqset #{(generate-term 'cq args)}]
      (if (< depth maxdepth)
        (recur (inc depth)
               (apply clojure.set/union
                      (map first
                           (map #(generate-entailment entailsym :antcount branching-factor :cqset #{%} :assert? true :args args) cqset))))
        (if args
          ;; Assert instances.
          (flatten
            (for [i inst-invert
                  cq cqset]
              (snuser/assert (list* (first cq) i))))
          ;; If there are no arbs, assert the leaves.
          (doall (map #(snuser/assert %) cqset))
          )))))

(defn generate-entailment-chain-antroot
  [entailsym branching-factor maxdepth arbcount rescount]
  (let [args (if (> arbcount 0)
               (for [i (range arbcount)]
                 (generate-arb rescount))
               nil)]
    (loop [depth 0
           antset #{(generate-term 'ant args)}]
      (if (< depth maxdepth)
        (recur (inc depth)
               (apply clojure.set/union
                      (map second 
                           (map #(generate-entailment entailsym :antset #{%} :cqcount branching-factor :assert? true :args args) antset))))
        antset))))

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

;;; Helper ;;;

(defn get-atom-or-molecular
  [name]
  (or (when (cf/FN2CF (symbol name)) (first @(:terms (cf/FN2CF (symbol name)))))
      (snuser/find-term (symbol name))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Graph Builders ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn write-entailment-graph-cqroot
  [entailsym bf depth arbcount rescount outfile]
  (snuser/clearkb true)
  (generate-entailment-chain-cqroot entailsym bf depth arbcount rescount)
  (print/writeKBToTextFile outfile))
    
(defn write-entailment-graph-antroot
  [entailsym bf depth arbcount rescount outfile]
  (snuser/clearkb true)
  (generate-entailment-chain-antroot entailsym bf depth arbcount rescount)
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
  [entailsym bf depth arbcount rescount]
  (reset-benchmark)
  (let [buildgraphfn #(generate-entailment-chain-antroot entailsym bf depth arbcount rescount)]
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
      (snip/backward-infer-derivable (get-atom-or-molecular 'cq) (ct/currentContext))
      (log-elapsed start-time)
      (print-time))))

(defn benchmark-bwd
  [entailsym bf depth arbcount rescount itrs]
  (def iterations itrs)
  (reset-benchmark)
  (let [buildgraphfn #(generate-entailment-chain-cqroot entailsym bf depth arbcount rescount)]
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
        (snip/backward-infer-derivable (get-atom-or-molecular 'cq) (ct/currentContext))
        (log-elapsed start-time)
        (print-time))
      (do
        (load-file fname)
        (let [start-time (. java.lang.System (clojure.core/nanoTime))]
          (snip/backward-infer-derivable (get-atom-or-molecular 'cq) (ct/currentContext))
          (log-elapsed start-time)
          (print-time))))))
