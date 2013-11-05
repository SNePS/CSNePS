(in-ns 'csneps.snip)

;; This is the worst case, combinatorial algorithm. 

(defrecord LinearRUISet
  [rui-set] ;;A ref to a set.
  RUIStructure
  (get-rule-use-info [this new-rui]
    ;; if new-rui isn't actually new, return empty set.
    (dosync
      (if (@(:rui-set this) new-rui)
        #{}
        (let [compat-ruis (filter #(compatible? % new-rui) @(:rui-set this))
              merged-ruis (set (map #(merge new-rui %) compat-ruis))]
          (alter (:rui-set this) union merged-ruis #{new-rui})
          (conj merged-ruis new-rui))))))

(defn make-linear-rui-set
  []
  (LinearRUISet. (ref #{})))