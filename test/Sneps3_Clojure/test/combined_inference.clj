(ns Sneps3-Clojure.test.combined-inference
  (:require [clojure.test :refer :all])
  (:require [csneps.core.snuser :as snuser])
  (:require [csneps.snip :as snip]))

(defn csneps-setup [f]
  (csneps.snip.inference-graph.concurrent/startExecutor)
  (snuser/nogoaltrace)
  (f))

(defn clearkb-fixture [f]
  (snuser/clearkb true)
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)

(deftest ig+path-novar
  (snip/definePath 'member '(compose member (kstar (compose equiv- ! equiv))))
  (snuser/assert '(Isa a q))
  (snuser/assert '(Equiv a b))
  (snuser/assert '(Equiv b c))
  (snuser/assert '(if (Isa c q) (Isa c r)))
  (is (= #{(snuser/defineTerm '(Isa c r))} (snuser/askif '(Isa c r)))))