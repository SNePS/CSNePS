(ns csneps.snip.messagestructure)

(defprotocol MessageStructure
  (get-new-messages [this new-msg])
  (seen-message? [this msg]) ;; Sometimes you don't want to combine msgs, just check if you've already seen one.
  (get-matched-messages [this])
  (get-sent-messages [this chtype])
  (add-matched-and-sent-messages [this matched sent] [this matched sent remove-matched-from-working?])
  (print-messages [this]))

(defmethod print-method csneps.snip.messagestructure.MessageStructure [o w]
  (.write ^java.io.Writer w
          (str (print-messages o))))

(prefer-method print-method csneps.snip.messagestructure.MessageStructure java.util.Map)
(prefer-method print-method csneps.snip.messagestructure.MessageStructure clojure.lang.IPersistentMap)
(prefer-method print-method csneps.snip.messagestructure.MessageStructure clojure.lang.IRecord)