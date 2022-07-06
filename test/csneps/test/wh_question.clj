(ns csneps.test.wh-question
  (:require [clojure.test :refer :all])
  (:require [csneps.core.snuser :as snuser]
            [csneps.core.find :as find]
            [csneps.core.contexts :as ct]))

(defn csneps-setup [f]
  (csneps.snip.inference-graph.concurrent/startExecutor)
  (snuser/nogoaltrace)
  (f))

(defn clearkb-fixture [f]
  (snuser/clearkb true)
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)
