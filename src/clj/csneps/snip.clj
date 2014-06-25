(ns csneps.snip
  (:require [csneps.core.build :as build]
            [csneps.core.find-utils :as find-utils]
            [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.relations :as slot]
            [clojure.walk :as walk])
;  (:refer-clojure :exclude [merge])
  (:use [csneps.core]
        [csneps.util]
        [csneps.snip.util]
        [csneps.core.build :only (term-prewalk variable?)]
        [clojure.core.memoize :only (memo)]
        [clojure.pprint :only (cl-format)]
        [clojure.set])
  (:import [java.util Comparator]
           [java.util.concurrent TimeUnit LinkedBlockingQueue PriorityBlockingQueue ThreadPoolExecutor]
           [edu.buffalo.csneps.util]))

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
(load "snip_originset")
(load "snip_message")
(load "snip_inference_graph")
(load "snip_acting")

(defn askif 
  "If the proposition prop is derivable in context,
      return a singleton set of that proposition;
      else return the empty set
        The termstack is a stack of propositions
           that this goal is a subgoal of.."
  [prop context termstack]
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
        (slot-based-derivable p context termstack)
        (backward-infer-derivable p context)))))

(defn askwh [ques context]
  "If the WhQuestion ques can be answered in context, 
      return a list of pairs, where the first element 
      is a satisfying term, and the second is a substitution.
      else return the empty set."
  (let [q (build/variable-parse-and-build ques :WhQuestion)]
    (vals (backward-infer-answer q context))))

(defn assertTrace
  [rule antecedents consequent reason context]
  (build/assert consequent context :der))