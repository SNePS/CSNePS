(ns Sneps3-Clojure.test.core
  (:use [core])
  (:use [clojure.test])
  (:require [Sneps3-Clojure.test.arithmetic]))


(defn load-sneps-fixture [f]
  (loadsneps3)
  (f))

(use-fixtures :once load-sneps-fixture)

(deftest test-suite
  (run-tests 'Sneps3-Clojure.test.arithmetic))