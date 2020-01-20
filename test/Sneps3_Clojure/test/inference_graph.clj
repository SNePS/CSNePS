(ns Sneps3-Clojure.test.inference-graph
  (:require [clojure.test :refer :all])
  (:require [csneps.core.snuser :as snuser]))

(defn csneps-setup [f]
  (csneps.snip.inference-graph.concurrent/startExecutor)
  (snuser/nogoaltrace)
  (f))

(defn clearkb-fixture [f]
  (snuser/clearkb true)
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)

(deftest atomic-implication-backward-infer
  (snuser/assert '(if a b))
  (snuser/assert 'a)
  (is (= #{(snuser/find-term 'b)} (snuser/askif 'b))))

(deftest generic-derivation-backward-infer
  (snuser/assert '(Isa (every x Cat) Animal))
  (snuser/assert '(Isa Glacier Cat))
  (snuser/assert '(Isa Fido Dog))
  (is (empty? (snuser/askif '(Isa Fido Animal))))
  (is (= #{(snuser/defineTerm '(Isa Glacier Animal))} (snuser/askif '(Isa Glacier Animal)))))

(deftest atomic-andor-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/defineTerm 'c)
  (snuser/defineTerm 'd)
  (snuser/defineTerm '(not e))
  ;;; And
  (let [andor (snuser/defineTerm '(and a b))]
    (is (= #{andor} (snuser/askif '(and a b))))
    (is (empty? (snuser/askif '(and a c)))))
  ;;; Or
  (let [andor-and (snuser/defineTerm '(or a b))
        andor-or (snuser/defineTerm '(or a c))]
    (is (= #{andor-and} (snuser/askif '(or a b))))
    (is (= #{andor-or} (snuser/askif '(or a c))))
    (is (empty? (snuser/askif '(or c d)))))
  ;; Xor
  (let [andor-xor (snuser/defineTerm '(xor a e))]
    (is (= #{andor-xor} (snuser/askif '(xor a e))))
    (is (empty? (snuser/askif '(xor a b)))) ;; False
    (is (empty? (snuser/askif '(xor a c)))))) ;; Unknown

(deftest atomic-thresh-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/assert '(not c))
  (snuser/assert '(not d))
  (let [thresh-iff1 (snuser/defineTerm '(iff a b))
        thresh-iff2 (snuser/defineTerm '(iff (not c) (not d)))]
    (is (= #{thresh-iff1} (snuser/askif '(iff a b))))
    (is (= #{thresh-iff2} (snuser/askif '(iff (not c) (not d)))))
    (is (empty? (snuser/askif '(iff a c))))))