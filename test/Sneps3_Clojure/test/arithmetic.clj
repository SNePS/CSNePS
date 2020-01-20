(ns Sneps3-Clojure.test.arithmetic
  (:use [csneps.core.arithmetic]
        [clojure.test])
  (:require [csneps.core.snuser :as snuser]))

(defn csneps-setup [f]
  (csneps.snip.inference-graph.concurrent/startExecutor)
  (f))

(defn clearkb-fixture [f]
  (snuser/clearkb true)
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)

(deftest add
  (is (= 1 (+ 0 1)))
  (is (= 7 (+ 2 1 4)))
  (is (= (symbol "1") (:name (+ 1 (snuser/defineTerm 0)))))
  (is (= (symbol "7") (:name (+ 2 1 (snuser/defineTerm 4))))))

(deftest sub
  (is (= -1 (- 0 1)))
  (is (= 1 (- 6 1 4)))
  (is (= (symbol "-1") (:name (- 0 (snuser/defineTerm 1)))))
  (is (= (symbol "1") (:name (- 6 1 (snuser/defineTerm 4))))))

(deftest mult
  (is (= 0 (* 0 1)))
  (is (= 8 (* 2 1 4)))
  (is (= (symbol "0") (:name (* 0 (snuser/defineTerm 1)))))
  (is (= (symbol "8") (:name (* 2 1 (snuser/defineTerm 4))))))

(deftest div
  (is (= 1 (/ 1 1)))
  (is (= 1/2 (/ 2 1 4)))
  (is (= (symbol "1") (:name (/ 1 (snuser/defineTerm 1)))))
  (is (= (symbol "1/2") (:name (/ 2 1 (snuser/defineTerm 4))))))

(deftest lt
  (is (= false (< 1 1)))
  (is (= true (< 1 2)))
  (is (= true (< 1 3 4)))
  (is (= false (< 1 3 5 4)))
  (is (= false (< 1 (snuser/defineTerm 0))))
  (is (= true (< 3 (snuser/defineTerm 4)))))

(deftest lteq
  (is (= true (<= 1 1)))
  (is (= true (<= 1 2)))
  (is (= true (<= 1 3 4)))
  (is (= false (<= 1 4 5 4)))
  (is (= false (<= 1 (snuser/defineTerm 0))))
  (is (= true (<= 4 (snuser/defineTerm 4)))))

(deftest gt
  (is (= false (> 1 1)))
  (is (= true (> 2 1)))
  (is (= true (> 4 3 1)))
  (is (= false (> 4 3 5 1)))
  (is (= true (> 1 (snuser/defineTerm 0))))
  (is (= false (> 3 (snuser/defineTerm 4)))))

(deftest gteq
  (is (= true (>= 1 1)))
  (is (= true (>= 2 1)))
  (is (= true (>= 4 3 1)))
  (is (= false (>= 4 3 4 1)))
  (is (= true (>= 1 (snuser/defineTerm 1))))
  (is (= false (>= 3 (snuser/defineTerm 4)))))

(deftest eq
  (is (= true (== 1 1)))
  (is (= false (== 1 2)))
  (is (= true (== 1 (snuser/defineTerm 1))))
  (is (= false (== 3 (snuser/defineTerm 4)))))

(deftest neq 
  (is (= true (not= 1 2)))
  (is (= false (not= 1 1)))
  (is (= false (not= 1 (snuser/defineTerm 1))))
  (is (= true (not= 3 (snuser/defineTerm 4)))))