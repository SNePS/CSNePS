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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Introduction Rules ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest consensus-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/defineTerm 'c)
  (snuser/assert '(not d))
  (snuser/assert '(not e))
  ;;; And
  (let [andterm (snuser/defineTerm '(and a b))]
    (is (= #{andterm} (snuser/askif '(and a b))))
    (is (empty? (snuser/askif '(and a c)))))
  ;; Nor
  (let [norterm (snuser/defineTerm '(nor d e))]
    (is (= #{norterm} (snuser/askif '(nor d e))))
    (is (empty? (snuser/askif '(nor d c))))))

(deftest disjunction-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/defineTerm 'c)
  (snuser/defineTerm 'd)
  (let [andor-and (snuser/defineTerm '(or a b))
        andor-or (snuser/defineTerm '(or a c))]
    (is (= #{andor-and} (snuser/askif '(or a b))))
    (is (= #{andor-or} (snuser/askif '(or a c))))
    (is (empty? (snuser/askif '(or c d))))))

(deftest xor-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/defineTerm 'c)
  (snuser/defineTerm '(not e))
  (let [andor-xor (snuser/defineTerm '(xor a e))]
    (is (= #{andor-xor} (snuser/askif '(xor a e))))
    (is (empty? (snuser/askif '(xor a b)))) ;; False
    (is (empty? (snuser/askif '(xor a c)))))) ;; Unknown

(deftest andor-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/assert 'c)
  (snuser/assert '(not f))
  (snuser/assert '(not g))
  (let [andor (snuser/defineTerm '(andor (3 5) a b c d e f g))]
    (is (= #{andor} (snuser/askif '(andor (3 5) a b c d e f g))))))

(deftest thresh-introduction
  (snuser/assert 'a)
  (snuser/assert 'b)
  (snuser/assert '(not c))
  (snuser/assert '(not d))
  (let [thresh-iff1 (snuser/defineTerm '(iff a b))
        thresh-iff2 (snuser/defineTerm '(iff (not c) (not d)))]
    (is (= #{thresh-iff1} (snuser/askif '(iff a b))))
    (is (= #{thresh-iff2} (snuser/askif '(iff (not c) (not d)))))
    (is (empty? (snuser/askif '(iff a c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elimination Rules ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest implication-elimination
  (snuser/assert '(if a b))
  (snuser/assert 'a)
  (is (= #{(snuser/find-term 'b)} (snuser/askif 'b))))

(deftest implication-elimination-chaining
  (snuser/assert 'a)
  (snuser/assert '(if a b))
  (snuser/assert '(if b c))
  (snuser/assert '(if c d))
  (is (= #{(snuser/find-term 'd)} (snuser/askif 'd))))

(deftest generic-elimination
  (snuser/assert '(Isa (every x Cat) Animal))
  (snuser/assert '(Isa Glacier Cat))
  (snuser/assert '(Isa Fido Dog))
  (is (empty? (snuser/askif '(Isa Fido Animal))))
  (is (= #{(snuser/defineTerm '(Isa Glacier Animal))} (snuser/askif '(Isa Glacier Animal)))))

(deftest negation-elimination
  (snuser/assert '(not (not a)))
  (is (= #{(snuser/defineTerm 'a)} (snuser/askif 'a)))
  (is (empty? (snuser/askif '(not a))))
  (snuser/assert '(not (not (not b))))
  (is (= #{(snuser/defineTerm '(not b))} (snuser/askif '(not b))))
  (is (empty? (snuser/askif 'b))))

(deftest conjunction-elimination
  (snuser/assert '(and a b c))
  (is (= #{(snuser/find-term 'a)} (snuser/askif 'a)))
  (is (= #{(snuser/find-term 'b)} (snuser/askif 'b)))
  (is (= #{(snuser/find-term 'c)} (snuser/askif 'c)))
  (is (empty? (snuser/askif 'd))))

(deftest disjunction-elimination
  (snuser/assert '(or a b c))
  (snuser/assert '(not a))
  (snuser/assert '(not b))
  (is (= #{(snuser/find-term 'c)} (snuser/askif 'c)))
  (is (empty? (snuser/askif 'b))))

(deftest xor-elimination
  (snuser/assert '(xor a b c))
  (snuser/assert 'a)
  (is (= #{(snuser/defineTerm '(not b))} (snuser/askif '(not b))))
  (is (empty? (snuser/askif '(not a)))))