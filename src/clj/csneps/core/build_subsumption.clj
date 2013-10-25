(in-ns 'csneps.core.build)

;;; There are many issues still to be worked out. 
;;; - Do we want to keep these separate?
;;; - What will be the role of unification?

(defn structurally-subsumes-vars
  "var1 structurally subsumes var2 if var1 has a subset of
   var2's restrictions."
  [var1 var2]
  (set/subset? (:restriction-set var1) (:restriction-set var2)))

(defn structurally-subsumes-terms
  "term1 structurally subsumes term2 if term1 is in a subset
   of the relations term2 is."
  [term1 term2]
  (let [t1rs @(:up-cablesetw term1)
        t2rs @(:up-cablesetw term2)]
    (every? 
      #(and (t2rs %)
            (set/subset? (t1rs %) (t2rs %)))
      (keys t1rs))))

(defn structurally-subsumes-varterm
  "var structurally subsumes term if term is in all of the
   relations that are in var's restriction set."
  [var term]
  (let [rs (:restriction-set var)
        ucs (:up-cablesetw term)]
    ))



