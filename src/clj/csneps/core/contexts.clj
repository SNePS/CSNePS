(ns csneps.core.contexts
;  (:require [sneps3-substitution :as subs])
  (:require [csneps.core :as csneps])
  (:use [csneps.util]))

(defvar CONTEXTS (ref (hash-map))
  "A map from context name to context.")

;;;PORTNOTE clj has no defparameter, which this used to be.
(defvar ^:dynamic *CurrentContext* (ref nil)
  "The current context.")

;;;PORTNOTE Missing the documentation string.. Also kinconsistent isnt typed (weird?)
(defrecord Context [
        ^clojure.lang.Symbol name
        ^String docstring
        ^clojure.lang.PersistentList parents
        ^clojure.lang.PersistentHashSet hyps
        ^clojure.lang.PersistentHashSet ders
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
  (assert (or (nil? parents) (seq? parents)))
  (assert (or (seq? hyps) (set? hyps)))
  (if (find-context name)
    (println "A context named" name "already exists.")
    (dosync (ref-set CONTEXTS (assoc @CONTEXTS name (Context. name docstring 
                                                              (doall (map #(find-context %) parents)) 
                                                              (ref (set (map #(csneps/get-term %) hyps))) 
                                                              (ref (hash-set)) false)))))
  (find-context name))

(defn currentContext
  "Returns the current context."
  []
  @*CurrentContext*)

(defn listContexts
  "Prints a list of all the contexts."
  []
  (loop [x 0]
    (if (< x (count @CONTEXTS)) 
      (do (println (nth (keys @CONTEXTS) x))
          (recur (+ 1 x)))
      nil)))

(defn remove-from-context
  "Removes the term from the context (ctx) hyps or ders."
  [term ctx]
  (dosync
    (alter (:hyps ctx) disj term)
    (alter (:ders ctx) disj term)))

(defn asserted?
  ""
  [p ct]
  (let [context (find-context ct)]
    (cond 
      (or (contains? @(:hyps context) p)
          (contains? @(:ders context) p))
      context
      ;; check parents
      :else (some #(asserted? p %) (:parents context)))))

;;Trace:
;context=> (defineContext {:name 'BaseCT})
;#'context/CONTEXTS
;context=> (set-current-context 'BaseCT)
;#'context/*CurrentContext*
;context=> *CurrentContext*
;#:context.Context{:name BaseCT, :docstring "", :parents (BaseCT), :hyps #{}, :ders #{}, :kinconsistent nil}
;context=> (listContexts)
;"BaseCT"




