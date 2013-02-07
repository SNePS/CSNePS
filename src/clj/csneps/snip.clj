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

(defmulti askif
  (fn [proposition context termstack] [(csneps/type-of proposition)]))

;  "If the proposition expressed by pname is derivable in context,
;        returns a singleton set of that proposition;
;        else returns the empty set.
;        The termstack is a stack of propositions
;            that this goal is a subgoal of."
(defmethod askif [clojure.lang.Symbol] [pname context termstack]
  (askif (build/build pname :Proposition {}) context termstack))

;  "If the proposition p is derivable in context,
;        return a singleton set of that proposition;
;        else return the empty set
;         The termstack is a stack of propositions
;             that this goal is a subgoal of.."
(defmethod askif [:csneps.core/Term] [p context termstack]
  (when GOALTRACE (cl-format true "~&I wonder if ~S~%" p))
  (cond
    (ct/asserted? p context)
    (do
      (when GOALTRACE (cl-format true "~&I know that ~S~%" p))
      #{p})
    :else
    (setOr
      (sort-based-derivable p context)
      ;; Slot-based inference uses path-based, so don't
      ;; waste time doing it twice.
      ;(path-based-derivable p context)
      (slot-based-derivable p context termstack)
      )))

(defn assertTrace
  [rule antecedents consequent reason context]
  (build/assert consequent context :der))