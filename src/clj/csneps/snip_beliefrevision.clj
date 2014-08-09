(in-ns 'csneps.snip)

(defn inconsistent?
  "If one of p1 or p2 is the negation of the other one, and both
   are believed in ct, then they are contradictory."
  [p1 p2 ct]
  (and (ct/asserted? p1 ct)
       (ct/asserted? p2 ct)
       (let [dcs-map1 (cf/dcsRelationTermsetMap p1)
             nor-dcs1 (when dcs-map1 (dcs-map1 (slot/find-slot 'nor)))
             dcs-map2 (cf/dcsRelationTermsetMap p2)
             nor-dcs2 (when dcs-map2 (dcs-map2 (slot/find-slot 'nor)))]
         (or (some #(= % p2) nor-dcs1)
             (some #(= % p1) nor-dcs2)))))

(defn detect-contradiction
  "When a change has been made to p1, this function can be used to 
   determine if p1 is now contradictory to some other term."
  [p1 ct]
  (and (ct/asserted? p1 ct)
       (let [dcs-map (cf/dcsRelationTermsetMap p1)
             nor-dcs (when dcs-map (dcs-map (slot/find-slot 'nor)))
             ucs-map (@up-cablesetw p1)
             nor-ucs (when ucs-map (ucs-map (slot/find-slot 'nor)))
             nor-ucs (when nor-ucs @nor-ucs)]
         (or (some #(ct/asserted? % ct) nor-dcs)
             (some #(ct/asserted? % ct) nor-ucs)))))