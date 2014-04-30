(in-ns 'csneps.core.build)

;;; This portion of csneps.core.build contains several
;;; rewrite rules. 

(defn add-rewrite
  [expr1 expr2]
  (println expr1 expr2)
  (assert (list 'thresh '(1 1) expr1 expr2) (ct/find-context 'BaseCT)))

(defn reverse-quantifier
  [expr var]
  (let [vg (gensym var)]
    (match/match [expr]
      [(['every v & r] :seq)] (when (= var v) (list* 'some vg '() (prewalk-replace {v vg} r)))
      [(['some v dep & r] :seq)] (when (= var v) (list* 'every vg (prewalk-replace {v vg} r)))
      :else nil)))

(defn find-and-reverse-quantifier
  [subexpr var]
  (prewalk #(do (if-let [rewrite (and (seq? %) (reverse-quantifier % var))]
                  rewrite
                  %))
         subexpr))

(defn match-propositional-expr 
  [expr]
  (match/match [expr]
    [(['close v r] :seq)] r
    [(['not (['thresh pn & r] :seq)] :seq)] (list* 'andor pn r)
    [(['not (['andor pn & r] :seq)] :seq)] (list* 'thresh pn r)
    [(['not (['close v r] :seq)] :seq)] (find-and-reverse-quantifier r v)
    :else nil))

(defn rewrite-propositional-expr
  [expr]
  (prewalk #(do (when (seq? %) 
                  (when-let [rewrite (match-propositional-expr %)]
                    (add-rewrite % rewrite))) 
              %)
         expr))

;(defn generate-rewrites
;  "Given a term, generates a set of rewrites, which
;   are all asserted using an if-and-only-if in the
;   base context."
;  [sexpr]
;  (let [rewrite-set #{}]
;    (when-not (empty? rewrite-set)
;      (assert (list 'thresh '(1 1) (conj rewrite-set sexpr)) (ct/find-context 'BaseCT)))))

