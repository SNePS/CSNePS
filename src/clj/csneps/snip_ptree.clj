(in-ns 'csneps.snip)




(defn PatVar-to-VarPat
  [term]
  ;; Antecedents of <term> can be found by following
  ;; i-channels backwards. So, filter the in-channels
  ;; of <term> to retrieve them.
  (let [in-i-ch (filter 
                  #(some (hash-set %) @(:i-channels (:originator %))) 
                  @(:in-channels term))
        ants (map #(:originator %) in-i-ch)
        vars (apply union (map variables-in ants))]
    
    
    
    
    (println vars)
  
  
  ))

