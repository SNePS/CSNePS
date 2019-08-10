(ns csneps.snip.util
  (:require [csneps.core :as csneps]
            [csneps.core.contexts :as ct]
            [csneps.core.build :as build]
            [clojure.set :as set])
  (:use [csneps.utils.coreutils]))

(defn variables-in
  [term]
  (loop [dcs #{term}
         vars #{}]
    (if (seq dcs)
      (if (variable? (first dcs))
        (recur (set (rest dcs)) (conj vars (first dcs)))
        (recur (apply set/union (set (rest dcs)) (:down-cableset (first dcs))) vars))
      vars)))