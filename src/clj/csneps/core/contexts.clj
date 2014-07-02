(ns csneps.core.contexts
  (:require [csneps.core :as csneps]
            [clojure.set :as set])
  (:use [csneps.util]))

(defvar CONTEXTS (ref (hash-map))
  "A map from context name to context.")

(defvar ^:dynamic *CurrentContext* (ref nil)
  "The current context.")

;;; "A SNePS 3 Context"
(defrecord Context [
        ^clojure.lang.Symbol name
        ^String docstring
        ^clojure.lang.PersistentList parents
        ^clojure.lang.PersistentHashSet hyps
        ^java.lang.Boolean kinconsistent])

(defn find-context
  "If ctx is a context, returns it.
     If ctx is a symbol, returns the context named ctx
      or nil if there isn't any."
  [ctx]
  (typecase ctx
    Context ctx
    clojure.lang.Symbol (get @CONTEXTS ctx)))

(defn setCurrentContext
  "If ctx is a context name,
         makes the context named ctx the current context.
     If ctx is a context, makes it the current context.
     Else throws an exception."
  [ctx]
  (assert (or (instance? Context ctx)
              (and (symbol? ctx) (find-context ctx))))
  (typecase ctx
    clojure.lang.Symbol (dosync (ref-set *CurrentContext* (find-context ctx)))
    Context (dosync (ref-set *CurrentContext* ctx))))

;;PORTNOTE: I changed hyps to init as (hash-set) and not nil.
(defn defineContext
  ""
  [name & {:keys [docstring parents hyps]
           :or {docstring "", parents '(BaseCT), hyps (hash-set)}}]
  (assert (symbol? name))
  (assert (string? docstring))
  (assert (or (nil? parents) (seqable? parents)))
  (assert (seqable? hyps) (set? hyps))
  (if (find-context name)
    (println "A context named" name "already exists.")
    (dosync (alter CONTEXTS assoc name (Context. name docstring 
                                                 (doall (map #(find-context %) parents)) 
                                                 (ref (set (map #(csneps/get-term %) hyps))) 
                                                 false))))
  (find-context name))

(defn currentContext
  "Returns the current context."
  []
  @*CurrentContext*)

(defn listContexts
  "Prints a list of all the contexts."
  []
  (doseq [n (keys @CONTEXTS)]
    (println n)))

(defn remove-from-context
  "Removes the term from the context (ct) hyps."
  [term ct]
  (dosync (alter (:hyps ct) disj term)))

(defn hyps
  "Returns the full set of hyps of ct, both those local to
   ct, and those of its parents."
  [ct]
  (loop [cts (list ct)
         hyps #{}]
    (if (empty? cts)
      hyps
      (recur (apply concat (map #(:parents %) cts))
             (set/union hyps (apply set/union (map #(deref (:hyps %)) cts)))))))

(defn asserted?
  "If p has an origin set which is a subset of the hyps of the
   context ct, then it is asserted."
  [p ct]
  (let [context (find-context ct)
        cthyps (hyps context)]
    (cond
      (cthyps p) 
      context
      (some 
        #(set/subset? (map csneps/get-term (second %)) cthyps)
        (filter #(not= (first %) 'hyp) (@csneps/support p)))
      context)))