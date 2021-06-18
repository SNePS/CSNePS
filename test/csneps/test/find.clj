(ns csneps.test.find
  (:use [csneps.core.find])
  (:require [clojure.test :refer :all]
            [csneps.core.snuser :as snuser]))

(defn initialize-types-and-frames []
  ;;; Define Types
  (snuser/defineType Agent (Thing) "Individuals that have agency")
  (snuser/defineType Action (Thing) "Actions that Agents can perform.")

  ;;; Define Slots
  (snuser/defineSlot agent :type Agent)
  (snuser/defineSlot actions :type Action)
  (snuser/defineSlot object :type Thing
                     :docstring "Non-agentive objects of actions.")
  (snuser/defineSlot property :type Thing)
  (snuser/defineSlot life :type Thing)
  (snuser/defineSlot whole :type Thing)
  (snuser/defineSlot part :type Thing)
  (snuser/defineSlot group :type Thing)

  ;;; Caseframes
  (snuser/defineCaseframe 'Proposition '(actions agent object)
                          :docstring "[agent] [actions] [object]"
                          :fsymbols '(Owns Buys)))

(defn csneps-setup [f]
  (csneps.snip.inference-graph.concurrent/startExecutor)
  (snuser/clearkb true)
  (initialize-types-and-frames)
  (f))

(defn clearkb-fixture [f]
  (snuser/clearkb false)
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)

(deftest find-base-prop
  (let [fido (snuser/assert '(Isa "Fido" Dog))
        rover (snuser/assert '(Isa Rover Dog))
        mister (snuser/assert '(Isa "Mister Meowgi" Cat))
        glacier (snuser/assert '(Isa Glacier Cat))]
    (is (= (list [fido {}]) (find '(Isa Fido Dog))))
    (is (= (list [rover {}]) (find '(Isa Rover Dog))))
    (is (= (list [mister {}]) (find '(Isa "Mister Meowgi" Cat))))
    (is (empty? (find '(Isa Lassie Dog))))))

(deftest find-by-variable
  (let [fido (snuser/assert '(Isa Fido Dog))
        rover (snuser/assert '(Isa Rover Dog))
        mister (snuser/assert '(Isa "Mister Meowgi" Cat))
        glacier (snuser/assert '(Isa Glacier Cat))]
    (is (empty? (find '(Isa x Dog)))) ;; x is not a variable here.
    (is (= (hash-set [fido {'x (snuser/find-term 'Fido)}]
                 [rover {'x (snuser/find-term 'Rover)}])
           (set (find '(Isa x Dog) '(x)))))
    (is (= (hash-set [fido {'?x (snuser/find-term 'Fido)}]
                     [rover {'?x (snuser/find-term 'Rover)}])
           (set (find '(Isa ?x Dog)))))))