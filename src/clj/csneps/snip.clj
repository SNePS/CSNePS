(ns csneps.snip
  (:require [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.core.find-utils :as find-utils]
            [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot])
  (:refer-clojure :exclude [merge])
  (:use [csneps.util]
        [csneps.snip.util]
        [csneps.core.build :only (term-prewalk variable?)]
        [clojure.pprint :only (cl-format)]
        [clojure.set])
  (:import [java.util Comparator]
           [java.util.concurrent TimeUnit LinkedBlockingQueue PriorityBlockingQueue ThreadPoolExecutor]))

(declare assertTrace askif)

(defvar TRACE nil
  "If non-nil, inference will be traced when rules fire.")

(defvar GOALTRACE true
  "If non-nil, inference will be traced
             when (sub)goals are generated,
             and when (sub)goals are found asserted in the KB.")

(load "snip_sort_based")
(load "snip_path_based")
(load "snip_slot_based")
(load "snip_rui")
(load "snip_inference_graph")

(defn askif [prop context termstack]
  (let [p (build/build prop :Proposition {})]
    (when GOALTRACE (cl-format true "~&I wonder if ~S~%" p))
    (cond
      (ct/asserted? p context)
      (do
        (when GOALTRACE (cl-format true "~&I know that ~S~%" p))
        #{p})
      :else
      (setOr
        (sort-based-derivable p context)
        (slot-based-derivable p context termstack)))))

(defn assertTrace
  [rule antecedents consequent reason context]
  (build/assert consequent context :der))