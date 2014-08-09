;;; This file includes several functions for calcluating origin sets. 
;;; Since IGs reason in all contexts simultaneously, it's often necessary to
;;; create origin sets which are the combinatoric combination of other sets,
;;; or to apply difference operations to many sets. These functions are meant
;;; to abstract that away from the implementation of IGs.

(in-ns 'csneps.snip)

(defn combine-origin-tags
  [t1 t2]
  (cond
    (= t1 'ext) 'ext
    (= t2 'ext) 'ext
    :else 'der))

(defn der-tag
  [ss]
  (set (map #(vector (combine-origin-tags (first %) 'der) (second %)) ss)))

(defn os-union 
  "When two sets of sets are unioned, the result is a set of sets 
   which has size |supports1|*|supports2|, and represents every 
   combination of the two."
  [supports1 supports2]
  (cond
    (empty? supports1) supports2
    (empty? supports2) supports1
    :else (set (for [[t1 os1] supports1
                     [t2 os2] supports2]
                 [(combine-origin-tags t1 t2) (union os1 os2)]))))

(defn os-remove-hyp
  "For all sets in supports1 which contain hyp,
   remove hyp, and return the resulting set of sets."
  [supports1 hyp]
  (let [sup-with-hyp (filter #(get (second %) hyp) supports1)]
    (set (map #(vector (first %) (disj (second %) hyp) sup-with-hyp)))))

(defn os-remove-hyps
  "For all sets in supports1 which are supersets of 
   hyps. remove the hyps elements, and return the 
   resulting set of sets."
  [supports1 hyps]
  (let [sup-with-hyps (filter #(subset? hyps (second %)) supports1)]
    (set (map #(vector (first %) (difference % hyps)) sup-with-hyps))))

(defn os-equal-sets
  "Return the set of sets in supports1 also in supports2 
   (i.e., the intersection of the two)"
  [supports1 supports2]
  (intersection supports1 supports2))

(defn os-concat
  "When two sets of OSes are concatted, the union of the two sets is
   taken, while ensuring only minimal members remain in the final 
   set."
  [supports1 supports2]
  (loop [result supports1
         supports2 (seq supports2)]
    (cond
      (empty? supports2) 
      result
      (nil? (first (filter #(subset? (second %) (second (first supports2))) supports1)))
      (recur (conj result (first supports2))
             (rest supports2))
      :else
      (recur result
             (rest supports2)))))

(defn alter-support
  [term new-support]
  (alter support assoc term new-support))
  
  