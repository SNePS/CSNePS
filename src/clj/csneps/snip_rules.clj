(in-ns 'csneps.snip)

(defn lhsrhs [body]
  (loop [body body
         lhs '()]
    (if (= (first body) '=>)
      [lhs (rest body)]
      (recur (rest body)
             (conj lhs (first body))))))

(defn formorsub [rhs]
  (loop [rhs rhs
         forms '()
         subrules '()]
    (cond
      (empty? rhs)
      [forms subrules]
      (= (ffirst rhs) :subrule)
      (recur (rest rhs)
             forms
             (conj subrules (first rhs)))
      :else
      (recur (rest rhs)
             (conj forms (first rhs))
             subrules))))
      

(defmacro defrule [rulename & body]
  (let [[lhs rhs] (lhsrhs body)
        [forms subs] (formorsub rhs)]
    `(build/build (list '~'rule '~rulename (set '~lhs) (set '~forms) (set '~subs)) :Policy {})))