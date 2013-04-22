(ns csneps.test.unification
  (:use [csneps.core.build])
  (:require [csneps.core.snuser]))

(defn basicunif-test []
  (addTermToUnificationTree (csneps.core.snuser/assert '(Isa (every x) Animal)))
  (getUnifiers (csneps.core.snuser/defineTerm '(Isa Glacier Animal))))

(defn fullunif-test []
  (csneps.core.snuser/clearkb true)
  (csneps.core.snuser/defineSlot entity :type Entity :docstring "General slot for holding entities.")
  (csneps.core.snuser/defineSlot entity1 :type Entity :docstring "General slot for holding entities.")
  (csneps.core.snuser/defineSlot entity2 :type Entity :docstring "General slot for holding entities.")
  (csneps.core.snuser/defineCaseframe 'Proposition '('SameSpecies entity entity1 entity2))
  (csneps.core.snuser/defineCaseframe 'Proposition '('caregiver entity1))
  (csneps.core.snuser/defineCaseframe 'Proposition '('friend entity2))
  (addTermToUnificationTree (csneps.core.snuser/assert '(SameSpecies (caregiver (every x)) (friend (caregiver (every w))) w)))
  (getUnifiers (csneps.core.snuser/defineTerm '(SameSpecies (every x) (friend x) Alex))))