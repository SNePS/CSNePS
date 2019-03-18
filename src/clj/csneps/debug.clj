(ns csneps.debug
  (:require [clojure.set :as set])
  (:use [csneps.core]))

;;;;;;;;;;;;;;;;;;;
;;; Debug Tools ;;;
;;;;;;;;;;;;;;;;;;;

(def screenprinter (agent nil))

(defn println-agent
  ([s]
   (send screenprinter (fn [_] (println s))))
  ([s & strs]
   (println-agent (clojure.string/join " " (conj strs s)))))

(def debug-features (ref #{}))
(def debug-nodes (ref #{}))

(defn set-debug-features [& opts]
  (dosync (ref-set debug-features (set opts))))

;; Empty nodes list means debug all nodes.
(defn set-debug-nodes [& nodes]
  (dosync (ref-set debug-nodes (set (map get-term nodes)))))

(defmacro debug [& {:keys [features nodes] :or {features '() nodes '()}}]
  `(do 
     (set-debug-features ~@features)
     (set-debug-nodes ~@nodes)))

(defmacro print-debug
  "Prints the message if any item from features is in debug-features, and
   if any of: any item in nodes is in debug-nodes or debug-nodes is empty
   or nodes is empty. Accepts either a set of nodes/features, or a single
   one on their own."
  [features nodes & message-strs]
  (let [features (if (seqable? features) (set features) #{features})
        nodes (if (and (seqable? nodes) (not (map? nodes))) (set nodes) #{nodes})] ;; Records are seqable.
    (when (and (not (empty? (set/intersection features @debug-features)))
               (or (empty? @debug-nodes)
                   (empty? nodes)
                   (not (empty? (set/intersection nodes @debug-nodes)))))
      `(println-agent ~@message-strs))))