(in-ns 'csneps.snip)

;; This is the worst case, combinatorial algorithm. 

(defn get-rule-use-info-linear
  "Finds all RUIs which are compatible with the new-rui. It then
   merges the new one with compatible ones, adds all newly created
   RUIs to the nodes rui-set, and returns the list of new RUIs."
  [rui-set-ref new-rui]
  ;; if new-rui isn't actually new, return empty set.
  (dosync
    (if (@rui-set-ref new-rui)
      #{}
      (let [compat-ruis (filter #(compatible? % new-rui) @rui-set-ref)
            merged-ruis (set (map #(merge new-rui %) compat-ruis))]
        (alter rui-set-ref union merged-ruis #{new-rui})
        (conj merged-ruis new-rui)))))