(in-ns 'csneps.snip)

(defn sort-based-derivable
  "If the categorization Proposition p
          is derivable in the given context
          accordng to sort-based inference
          returns a singleton set of that proposition;
     else returns the empty set ."
  [p context]
  ;; sort-based-derivable only considers the sorts of terms.
  ;; So it doesn't consider any term that logically implies p.
  ;; So it doesn't need a termstack argument.
  (if-not (= (syntactic-type-of p) :csneps.core/Categorization)
    #{}
    (let [members (seq (find-utils/findto p 'member))]
      (if (= (count members)
                (loop [member members
                       countm 0]
                  (let [classes (find-utils/findto p 'class)]
                    (if (and member (= (count classes)
                            (loop [class classes
                                   countc 0]
                              (if (and class
                                       (semantic-type-p (keyword (:name (first class))))
                                       (isa? (st/semantic-type-of (first member)) (keyword (:name (first class)))))
                                (recur (next class) (inc countc))
                                countc))))
                      (recur (next member) (inc countm))
                      countm))))
        (do
          (assertTrace nil nil p "Sort-Based inference" context)
          #{p})
        #{}))))