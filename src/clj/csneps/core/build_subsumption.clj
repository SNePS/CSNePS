(in-ns 'csneps.core.build)

;;; There are many issues still to be worked out. 
;;; - Do we want to keep these separate?
;;; - What will be the role of unification?

(defn structurally-subsumes-vars
  "var1 structurally subsumes var2 if var1 has a subset of
   var2's restrictions."
  [var1 var2])

(defn structurally-subsumes-terms
  "term1 structurally subsumes term2 if term2 is in a subset
   of the relations term1 is."
  [term1 term2])

(defn structurally-subsumes-varterm
  "var structurally subsumes term if term is in all of the
   relations that are in var's restriction set."
  [var term])