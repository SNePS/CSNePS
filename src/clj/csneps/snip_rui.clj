;; Based on work by Choi '92

(in-ns 'csneps.snip)

(defrecord2 Rui
  [subst {}        ;; Substitution
   pos 0           ;; Number of known positive ants
   neg 0           ;; Number of known negative ants
   support-set #{} ;; Support set for the given substitution
   flaggedns {}])  ;; A map from term to truth value

(defn rui-from-message
  "Creates an RUI from a message."
  [message]
  (new-rui {:subst (:substitution message)
            :pos (if (:true? message) 1 0)
            :neg (if (:true? message) 0 1)
            :support-set (:support-set message)
            :flaggedns {(:origin message) (:true? message)}}))


(defn rui-key [rui]
  (set (keys (:subst rui))))

(defn compatible? [rui1 rui2]
  "Returns true if the two RUIs do not have contradictory 
   flagged node sets."
  (loop [fns1 (:flaggedns rui1)]
    (if (empty? fns1) 
      true
      (let [fn1p (second fns1)
            fn2p (get (:flaggedns rui2) (first fns1))]
        (if (or 
              (and (false? fn2p) (true? fn1p))
              (and (true? fn2p) (false? fn1p)))
          nil
          (recur (rest fns1)))))))

(defn merge [rui1 rui2]
  (new-rui {:subst (build/substitution-composition (:subst rui1) (:subst rui2))
            :pos (+ (:pos rui1) (:pos rui2))
            :neg (+ (:neg rui1) (:neg rui2))
            :support-set (union (:support-set rui1) (:support-set rui2))
            :flaggedns (clojure.core/merge (:flaggedns rui1) (:flaggedns rui2))}))
  
  
  