(ns CSNePS.test.core
  (:use [clojure.test])
  (:require [CSNePS.test.arithmetic]))


(defn load-sneps-fixture [f]
  (f))

(use-fixtures :once load-sneps-fixture)

;(deftest test-suite
;  (run-tests 'CSNePS.test.arithmetic))