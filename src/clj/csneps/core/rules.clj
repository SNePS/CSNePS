(in-ns 'csneps.core.snuser)

(defn subs-non-locals
  [pattern vars subs]
  (postwalk #(if (and (build/synvariable? %)
                      (not (some #{%} vars)))
               (subs %)
               %)
            pattern))

(defn fix-forms
  [forms subs]
  (for [f forms] (replace {'of subs} f)))

(defmacro withInstances 
  "For each asserted substitution instance of pattern, evaluates the forms in forms,
      with each variable in variables
         taking on the term appropriate for the instance.
   Question mark variables in pattern that are not in variables
      take on the values they should have gotten in an enclosing withInstances."
  [vars of pattern & forms]
  (let [subs (when (map? of) of)
        pattern (if subs (subs-non-locals pattern vars subs) pattern)
        res (map second (find pattern vars))]
    `(do
       ~@(for [r res]
           (let [rnobj (into {} (for [[k v] r] [k (:name v)]))
                 varmap (merge subs (zipmap vars (for [v vars] (get rnobj v))))
                 larg (vec (interleave vars (for [v vars] `(get '~rnobj '~v))))]
             `(let ~larg
                ~@(fix-forms forms varmap)))))))

(defn build-rule-fn [rhs]
  ;; Builds a partial function which takes a substitution
  ;; and rule RHS. That function instantiates the RHS for
  ;; execution.
  )

(defn lhsrhs [body]
  (loop [body body
         lhs '()]
    (if (= (first body) '=>)
      [lhs (rest body)]
      (recur (rest body)
             (conj lhs (first body))))))

(defmacro defrule [rulename & body]
  (let [[lhs rhs] (lhsrhs body)]
    `(defineTerm (list '~'rule (set '~lhs) (set '~rhs)))))