(ns csneps.core
  (:require [clojure.pprint]
            [clojure.set]
            [clojure.string :as st])
  (:use [csneps.util]))

;; Load the rest of the csneps.core namespace.
(load "core_syntactic_types")
(load "core_semantic_types")