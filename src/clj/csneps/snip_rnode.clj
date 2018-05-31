(in-ns 'csneps.snip)

(defrecord2 rnode
  [term         nil
   cached-terms (ref #{})
   origin-set (ref #{})])
  