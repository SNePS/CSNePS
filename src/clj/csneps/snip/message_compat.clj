(ns csneps.snip.message-compat
  (:use [clojure.set]
        [csneps.util]
        [csneps.snip.message])
  (:require [csneps.core.build :as build]))

(defn compatible? [msg1 msg2]
  "Returns true if the two Messages do not have contradictory
   flagged node sets, and their substitutions are compatible."
  (and
    (build/compatible-substitutions? (:subst msg1) (:subst msg2))
    (loop [fns1 (:flaggedns msg1)]
      (if (empty? fns1)
        true
        (let [fn1p (second fns1)
              fn2p (get (:flaggedns msg2) (first fns1))]
          (when-not (or
                      (and (false? fn2p) (true? fn1p))
                      (and (true? fn2p) (false? fn1p)))
            (recur (rest fns1))))))))