
(ns csneps.core.relations
  (:use [csneps.util])
  (:require [csneps.core]))

(def SLOTS (ref (hash-map)))

(defrecord2 Slot
  [name (gensym "rel")  ;the name of the slot
   type :Entity         ;the type of terms it points to
   docstring ""         ;A documentation string for the slot
   posadjust 'reduce    ;for slot-based inference: reduce, expend, or none
   negadjust 'expand    ;for slot-based inference of negative instances
   min 1                ;minimum number of slot fillers
   max nil              ;maximum number of slot fillers, nil means infinite
   path (ref nil)             ;the path that implies this slot
   f-pathfn (ref nil)         ;"forward" path function
   b-pathfn (ref nil)])       ;"backward" path function

(defmethod print-method csneps.core.relations.Slot [o w]
  (.write ^java.io.Writer w (str "name: " (:name o)
                                 "\n\tdocstring: " (:docstring o)
                                 "\n\ttype: " (:type o)
                                 "\n\tmin: " (:min o) " max: " (:max o) "\tposadjust: " (:posadjust o) " negadjust: " (:negadjust o) "\n")))

(defn find-slot
  "If rname is a slot, returns it;
     if it is the name of a slot, returns the slot object;
     else if errorp is True, raises an errorr
          else returns nil."
  [rname & {:keys [errorp] :or {errorp true}}]
  (typecase rname
    Slot rname
    clojure.lang.Symbol
      (let [slot (get @SLOTS rname)]
        (if (nil? slot)
          (when errorp (error (str "There is no slot named " rname)))
          slot))))

(defn define-slot
  "Defines a slot"
  [name & {:keys [type docstring posadjust negadjust min max path]
           :or {type :Entity,
                docstring "",
                posadjust 'reduce,
                negadjust 'expand,
                min 1
                path (ref nil)}}]
  {:pre [(symbol? name)]} ;"Slot name must be a symbol."
  (cond
    (find-slot name :errorp false)
    (do
      (println "A slot with name: " name " is already defined. Using existing definition.")
      (find-slot name))
    (= name 'restriction)
    (error (str "'" name "' is a reserved word and not a valid name for a slot."))
    :else
    (do
      (assert (csneps.core/semantic-type-p (keyword type))) ;"The type given is not a valid semantic type"
      (assert (string? docstring))
      (assert (some (hash-set posadjust) '(reduce expand none)))
      (assert (some (hash-set negadjust) '(reduce expand none)))
      (assert (and (integer? min) (>= min 0)))
      (assert (or (nil? max)
                  (and (integer? max) (>= max min))))
      (let [newslot (new-slot {:name name :type (keyword type) :docstring docstring :posadjust posadjust :negadjust negadjust :min min :max max :path (ref path)})]
        (dosync
          (alter SLOTS assoc name newslot))
      newslot))))

(defn list-slots
  "Prints a list of all the SNePS slots"
  []
  (doseq [s (vals @SLOTS)]
    (println s)))