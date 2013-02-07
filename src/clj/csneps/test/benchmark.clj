(ns csneps.test.benchmark
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.snip :as snip]
            [clojure.set :as set]
            [csneps.core.snuser :as snuser]))

(def ^:dynamic start-time nil)
(def ^:dynamic end-time nil)

(declare benchmark-impl statistics)

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

(defn generate-implication
  [assert? ant-ct cqset or?]
  (let [ants (set (map gensym (repeat ant-ct "ant")))]
    (if assert?
      [ants (snuser/assert (list (if or? '=v> 'if) ants cqset))]
      [ants (snuser/defineTerm (list (if or? '=v> 'if) ants cqset))])))

(defn generate-implication-chain
  [branching-factor maxdepth or?]
  (loop [depth 0
         cqset '#{cq}]
    (if (< depth maxdepth)
      (recur (inc depth)
             (apply clojure.set/union
                    (map first 
                         (map #(generate-implication true branching-factor % or?) cqset))))
      (doall (map #(snuser/assert %) cqset)))))

(def totaltime (atom 0))
(def iterations (atom 10))

(defn log-elapsed
  [start-time]
  (swap! totaltime + (/ (- (. java.lang.System (clojure.core/nanoTime)) start-time) 1000000.0)))


(defn print-elapsed
  [start-time]
  (println "Elapsed:"
           (/ (- (. java.lang.System (clojure.core/nanoTime)) start-time) 1000000.0) "ms"))


(defn benchmark-done?
  [bench start-time ref key oldvalue newvalue]
  (when (and (newvalue (snuser/find-term 'cq))
             (not (oldvalue (snuser/find-term 'cq))))
    (log-elapsed start-time)
    (println "Done." @iterations)
    (statistics)
    (remove-watch (:ders (ct/currentContext)) :ders)
    (if (> (swap! iterations dec) 0)
      (bench)
      (println "Total Time:" @totaltime)
    )))

(defn benchmark-impl
  []
  (snuser/clearkb false)
  (generate-implication-chain 2 10 false)
  (let [start-time (. java.lang.System (clojure.core/nanoTime))]
    (add-watch (:ders (ct/currentContext)) :ders (partial benchmark-done? benchmark-impl start-time))
    (snip/backward-infer (snuser/find-term 'cq))))

(defn benchmark
  []
  (def totaltime (atom 0))
  (def iterations (atom 10))
  (benchmark-impl))

(defn benchmark-done-s?
  [start-time ref key oldvalue newvalue]
  (when (and (newvalue (snuser/find-term 'cq))
             (not (oldvalue (snuser/find-term 'cq))))
    (log-elapsed start-time)
    ;(println "Total Time:" @totaltime)
    ;(println "Done.")
    ;(statistics)
    (remove-watch (:ders (ct/currentContext)) :ders)))
    (if (> (swap! iterations dec) 0)
      (println "Total Time:" @totaltime)
    )

  (def iterations (atom 100))
    
(defn benchmark-test
  [bench iters]
  (def totaltime (atom 0))
  (def iterations (atom iters))
  (bench))
  
(defn fwd-inf-chain
  []
  ;(def totaltime (atom 0))
  (snuser/clearkb)
  (snuser/assert '(=v> (setof ant1 ant2) ant3))
  (snuser/assert '(=v> (setof ant3 ant4) ant5))
  (snuser/assert '(=v> (setof ant5 ant6) ant7))
  (snuser/assert '(=v> (setof ant7 ant9) ant9))
  (snuser/assert '(=v> (setof ant9 ant2) ant11))
  (snuser/assert '(=v> (setof ant11 ant2) ant13))
  (snuser/assert '(=v> (setof ant13 ant2) ant15))
  (snuser/assert '(=v> (setof ant15 ant2) ant17))
  (snuser/assert '(=v> (setof ant17 ant2) ant19))
  (snuser/assert '(=v> (setof ant19 ant2) cq))
  (let [start-time (. java.lang.System (clojure.core/nanoTime))]
    (add-watch (:ders (ct/currentContext)) :ders (partial benchmark-done? fwd-inf-chain start-time))
    (snuser/assert! 'ant1)))


(defn generate-next-level
  [true-cqs false-cqs]
  
  
  
  )







(defn statistics
  []
  (let [molterms (filter csneps/molecularTerm? (vals @csneps/TERMS))
        inferredin (filter #(not (nil? @(:ruis %))) molterms)]
    (println "Molecular Terms:" (count molterms))
    (println "Terms Inferred In:" (count inferredin))))

(defn generate-graph
  [depth order]
  
  
  )

    
    
    

  
  (defn ttassert
  []
  (snuser/clearkb true)
  (let [nums (range 1 1023)
        ants (map #(str "ant" %) nums)]
    (doall (map #(snuser/defineTerm %) ants))
    (time (doall (map #(build/assert % (ct/currentContext) :hyp) ants)))
    nil))
  

