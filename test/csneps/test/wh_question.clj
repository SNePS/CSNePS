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
  (snuser/defineType BFOEntity (Thing))
  (snuser/defineType Continuant (BFOEntity))
  (snuser/defineType Occurrent (BFOEntity))
  (snuser/defineType IndependentContinuant (Continuant))
  (snuser/defineType SpecificallyDependentContinuant (Continuant))
  (snuser/defineType GenericallyDependentContinuant (Continuant))
  (snuser/defineType MaterialEntity (IndependentContinuant))
  (snuser/defineType Object (MaterialEntity))
  (snuser/defineType FiatObjectPart (MaterialEntity))
  (snuser/defineType ObjectAggregate (MaterialEntity))
  (snuser/defineType ImmaterialEntity (IndependentContinuant))
  (snuser/defineType Site (ImmaterialEntity))
  (snuser/defineType ContinuantFiatBoundary (ImmaterialEntity))
  (snuser/defineType SpatialRegion (ImmaterialEntity))
  (snuser/defineType ZeroDimensionalContinuantFiatBoundary (ContinuantFiatBoundary))
  (snuser/defineType OneDimensionalContinuantFiatBoundary (ContinuantFiatBoundary))
  (snuser/defineType TwoDimensionalContinuantFiatBoundary (ContinuantFiatBoundary))
  (snuser/defineType ZeroDimensionalSpatialRegion (SpatialRegion))
  (snuser/defineType OneDimensionalSpatialRegion (SpatialRegion))
  (snuser/defineType TwoDimensionalSpatialRegion (SpatialRegion))
  (snuser/defineType ThreeDimensionalSpatialRegion (SpatialRegion))
  (snuser/defineType Quality (SpecificallyDependentContinuant))
  (snuser/defineType RealizableEntity (SpecificallyDependentContinuant))
  (snuser/defineType Role (RealizableEntity))
  (snuser/defineType Disposition (RealizableEntity))
  (snuser/defineType Function (Disposition))

  (snuser/defineType Process (Occurrent))
  (snuser/defineType ProcessBoundary (Occurrent))
  (snuser/defineType TemporalRegion (Occurrent))
  (snuser/defineType SpatioTemporalRegion (Occurrent))
  (snuser/defineType ZeroDimensionalTemporalRegion (TemporalRegion))
  (snuser/defineType OneDimensionalTemporalRegion (TemporalRegion))

  (snuser/defineType Color (Quality))
  (snuser/defineType GrayColor (Color))

  (snuser/defineType LivingBeing (Object))
  (snuser/defineType Animal (LivingBeing))
  (snuser/defineType Dog (Animal))
  (snuser/defineType Friendly (Disposition))
  (snuser/defineType Pet (Role))

  (snuser/defineSlot ics :type IndependentContinuant)
  (snuser/defineSlot sdcs :type SpecificallyDependentContinuant)
  (snuser/defineCaseframe 'Proposition '('BearerOf ics sdcs))
  (snuser/assert '(Isa rex Dog))
  (snuser/assert '(Isa grayColorOfRex GrayColor))
  (snuser/assert '(BearerOf rex grayColorOfRex))
  (f))

(use-fixtures :once csneps-setup)
(use-fixtures :each clearkb-fixture)

(deftest one-qvar-tests
  (let [ans (snuser/askwh '(Isa (?y SpecificallyDependentContinuant) GrayColor))
        qvar1 (snuser/find-term 'qvar1)
        grayColorOfRex (snuser/find-term 'grayColorOfRex)]
    (is (= (count ans) 1))
    (is (= ans (list {qvar1, grayColorOfRex}))))
  (let [ans (snuser/askwh '(BearerOf rex (?x SpecificallyDependentContinuant)))
        qvar1 (snuser/find-term 'qvar1)
        grayColorOfRex (snuser/find-term 'grayColorOfRex)]
    (is (= (count ans) 1))
    (is (= ans (list {qvar1, grayColorOfRex}))))
  (let [ans (snuser/askwh '(BearerOf (?x IndependentContinuant) grayColorOfRex))
        qvar2 (snuser/find-term 'qvar2)
        rex (snuser/find-term 'rex)]
    (is (= (count ans) 1))
    (is (= ans (list {qvar2, rex})))))

(deftest two-qvar-tests
  (let [ans (snuser/askwh '(BearerOf (?x IndependentContinuant) (?y SpecificallyDependentContinuant)))
        qvar1 (snuser/find-term 'qvar1)
        qvar2 (snuser/find-term 'qvar2)
        rex (snuser/find-term 'rex)
        grayColorOfRex (snuser/find-term 'grayColorOfRex)]
    (is (= (count ans) 1))
    (is (= (count (first ans)) 2))
    (is (= ans (list {qvar1 rex, qvar2 grayColorOfRex})))))

(deftest multi-restrict-qvar-tests
  (let [ans (snuser/askwh '(BearerOf (?x IndependentContinuant) (?y SpecificallyDependentContinuant (Isa ?y GrayColor))))
        qvar1 (snuser/find-term 'qvar1)
        qvar2 (snuser/find-term 'qvar2)
        rex (snuser/find-term 'rex)
        grayColorOfRex (snuser/find-term 'grayColorOfRex)]
    (is (= (count ans) 1))
    (is (= (count (first ans)) 2))
    (is (= ans (list {qvar1 rex, qvar2 grayColorOfRex})))))