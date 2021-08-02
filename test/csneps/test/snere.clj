(ns csneps.test.snere
  (:require [clojure.test :refer :all]
            [csneps.core.snuser :as snuser]
            [csneps.snip :as snip])
  (:use [clojure.pprint :only (cl-format)]))

(defn csneps-setup [f]
  (csneps.snip.inference-graph.concurrent/startExecutor)
  (snuser/nogoaltrace)
  (f))

(defn clearkb-fixture [f]
  (snuser/clearkb true)
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Actions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest primitive-action
  (let [act (snuser/defineTerm 'helloWorld :Act)
        primaction-fn (snip/define-primaction helloWorldfn []
                                              (cl-format true "~&Hello world.~%"))]
    (snip/attach-primaction act primaction-fn)))
    ;(snip/perform 'helloWorld)))