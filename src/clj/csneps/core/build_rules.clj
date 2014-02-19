(in-ns 'csneps.core.build)

(declare defrule)

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

(defn defrule-helper [rulename body substitutions]
  (let [[lhs rhs] (lhsrhs body)
        [forms subrules] (formorsub rhs)]
    (build (list 'rule rulename (set lhs) (set forms) subrules) :Policy substitutions)))

(defmacro defrule [rulename & body]
  `(defrule-helper '~rulename '~body {}))