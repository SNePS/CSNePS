(ns csneps.snip
  (:require [clojure.string :as string]
            [csneps.core.build :as build]
            [csneps.core.find-utils :as find-utils]
            [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.find :as find]
            [csneps.core.relations :as slot]
            [csneps.core.semantic-types :as st]
            [csneps.core.printer :as print]
            [csneps.snip.message :as msg]
            [csneps.snip.messagestructure :as msgstruct]
            [csneps.snip.originset :as os]
            [csneps.snip.ptree :as ptree]
            [csneps.snip.linear_message_set :as lms]
            [csneps.snip.passthrough-message-set :as pms]
            [csneps.snip.sindex :as sindex]
            [clojure.walk :as walk])
;  (:refer-clojure :exclude [merge])
  (:use [csneps.core]
        [csneps.util]
        [csneps.utils.coreutils]
        [csneps.configuration]
        [csneps.debug]
        [csneps.snip.util]
        [csneps.snip.inference-graph.concurrent]
        [csneps.core.build :only (term-prewalk)]
        [clojure.core.memoize :only (memo)]
        [clojure.pprint :only (cl-format)]
        [clojure.set])
  (:import [java.util Comparator]
           [java.util.concurrent TimeUnit LinkedBlockingQueue PriorityBlockingQueue ThreadPoolExecutor]
           [edu.buffalo.csneps.util CountingLatch]))

(declare assertTrace askif)

(def trace
  "If non-nil, inference will be traced when rules fire."
  (atom nil))

(def goaltrace
  "If non-nil, inference will be traced
     when (sub)goals are generated,
       and when (sub)goals are found asserted in the KB."
  (atom true))

(load "snip_sort_based")
(load "snip_path_based")
(load "snip_slot_based")
;(load "snip_originset")
;(load "snip_message")
(load "snip_inference_graph")
(load "snip_acting")

(defn askif 
  "If the proposition prop is derivable in context,
      return a singleton set of that proposition;
      else return the empty set
        The termstack is a stack of propositions
           that this goal is a subgoal of.."
  [prop context termstack]
  (let [p (build/build prop :Proposition {} #{})]
    (when @goaltrace (cl-format true "~&I wonder if ~S~%" p))
    (cond
      (ct/asserted? p context)
      (do
        (when @goaltrace (cl-format true "~&I know that ~S~%" p))
        #{p})
      :else
      (setOr
        (slot-based-derivable p context termstack)
        (backward-infer-derivable p context)
        (when-not semtype-objectlang-experimental (sort-based-derivable p context))
        ))))

(defn askwh [ques context]
  "If the WhQuestion ques can be answered in context, 
      return a list of substitutions for the qvars,
      else return the empty set."
  (let [q (build/variable-parse-and-build ques :Entity #{})]
    (for [[k v] (backward-infer-answer q context)
          :when (not (analyticTerm? v))]
      k)))

(defn askwh-instances [ques context]
  "If the WhQuestion ques can be answered in context, 
      return a list of satisfying terms,
      else return the empty set."
  (let [q (build/variable-parse-and-build ques :Entity #{})]
    (set (remove analyticTerm? (vals (backward-infer-answer q context))))))

(defn assertTrace
  [rule antecedents consequent reason context]
  (build/assert consequent context))