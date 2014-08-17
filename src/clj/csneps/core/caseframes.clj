(ns csneps.core.caseframes
  (:use [csneps.util])
  (:require [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [csneps.core.find-utils]))

(def CASEFRAMES 
  "A set of all the caseframes." 
  (ref (hash-set)))

(def FN2CF 
  "Map from function name to caseframe."
  (ref (hash-map)))

(def FrameAliases {'not 'nor
                   'iff 'thresh
                   'xor 'andor
                   'nand 'andor})

(def NoviceCaseframes 
  "Map from the number of slots of a novice caseframe
           to the novice caseframe."
  (ref (hash-map)))

(defrecord2 Caseframe
  [type nil
   docstring ""
   descfun nil
   slots nil
   print-pattern nil
   adj-to (ref (hash-set))
   adj-from (ref (hash-set))
   terms (ref (hash-set))])

(defn caseframe-name
  [cf]
  "This returns the name of the cf, but if the cf doesn't have a name
  (ie. it has a slot in its first position), it returns the name of
  that slot."
  (if (seq? (first (:print-pattern cf)))
        (second (first (:print-pattern cf)))
        (first (:print-pattern cf))))

(defmethod print-method csneps.core.caseframes.Caseframe [o w]
  (.write ^java.io.Writer w (str "caseframe: " (caseframe-name o)
                                 "\n\ttype: " (:type o)
                                 "\n\tslots: " (string/join (for [s (:slots o)] (str "\t" (:name s) "\n\t"))))))

(defn find-frame
  [fname]
  (or 
    (get @FN2CF fname)
    (get @FN2CF (FrameAliases fname))))

(defn description
  [term]
  (cond
    (csneps/atomicTerm? term) (str (:name term))
    (csneps/molecularTerm? term) ((:descfun (@csneps/caseframe term)) term)
    (set? term) (clojure.string/join ", and " (for [trm term] (description trm)))))

(defn make-description-function
  "Parses the docstring of the caseframe cf,
     and creates a function that returns a description for the
     case-frame arcs."
  [desc-string]
  (let [arcstrs (re-seq #"\[.*?\]" desc-string)
        arc-list (map #(slot/find-slot (symbol (.substring ^String % 1 (dec (.length ^String %))))) arcstrs)
        newdescstr (clojure.string/replace desc-string #"\[.*?\]" "~A")]
    (fn [n]
        (apply clojure.pprint/cl-format nil newdescstr
               (doall (for [arc arc-list] (description (csneps.core.find-utils/findto n arc))))))))

(defn add-fn-cf-map
  "Adds the map from the function name fn to the caseframe cf.
     If fn is a list, (f), the map is from the caseframe f is mapped to to cf."
  [fn cf]
  (if (seq? fn)
    (do
      (when-not (find-frame (first fn))
        (throw Exception (str (first fn) "does not have a caseframe defined for it.")))
      (dosync (alter FN2CF assoc (find-frame (first fn)) cf)))
    (dosync (alter FN2CF assoc fn cf))))

(defn pos-adj
  "Returns t if srcframe is a caseframe
         that is pos-adjustable to the caseframe tgtframe"
  [srcframe tgtframe]
  (or (= srcframe tgtframe)
      (let [srcslots  (:slots srcframe)
            tgtslots  (:slots tgtframe)]
        ;; CF <C_src,R_src> is pos-adjustable to case frame <C_tgt,R_tgt> iff:
        (and
          ;; 1) C_src is the same as, or a subtype of, C_tgt
          (csneps/subtypep (csneps/type-of (:type srcframe))
                                (csneps/type-of (:type tgtframe)))
          ;; 2) Every slot in R_src - R_tgt is posadjust reducible and has min = 0
          (every? #(or   (find % tgtslots)
                         (and (zero? (:min %))
                              (= (:posadjust %) 'reduce)))
                  srcslots)
          ;; 3) Every slot in R_tgt - R_src is posadjust expandable and has min = 0
          (every? #(or    (find % srcslots)
                          (and (zero? (:min %))
                               (= (:posadjust %) 'expand)))
                  tgtslots)))))

(defn neg-adj
  "Returns t if srcframe is a caseframe
        that is neg-adjustable to the caseframe tgtframe"
  [srcframe tgtframe]
  (or (= srcframe tgtframe)
      (let [srcslots  (:slots srcframe)
            tgtslots  (:slots tgtframe)]
        ;; Case frame <C_src,R_src> is neg-adjustable to case frame <C_tgt,R_tgt> iff:
        (and
          ;; 1) C_src is the same as, or a subtype of, C_tgt
          (csneps/subtypep (csneps/type-of  (:type srcframe))
                                (csneps/type-of  (:type tgtframe)))
          ;; 2) Every slot in R_src - R_tgt is negadjust reducible and has min = 0
          (every? #(or (find % tgtslots)
                       (and (zero? (:min %))
                            (= (:negadjust %)
                               'reduce)))
                  srcslots)
          ;; 3) Every slot in R_tgt - R_src is nrgadjust expandable and has min = 0
          (every? #(or (find % srcslots)
                       (and (zero? (:min %))
                            (=  (:negadjust %)
                                'expand)))
                  tgtslots)))))

(defn pseudo-adjustable
  "Returns t if srcframe is 'pseudo-adjustable' to tgtframe.
       Pseudo-adjustability allows slot-based inference to operate on frames
       that are not actually adjustable, e.g. nor and andor"
  [srcframe tgtframe]
  (cond
   ;; nor pseudo-adjusts to andor
   (= srcframe (find-frame 'nor))  (= tgtframe (find-frame 'andor))))

(defn adjustable?
  "Returns true if srcframe is a caseframe that is adjustable
         to the caseframe tgtframe"
  [srcframe tgtframe]
  (or (pos-adj srcframe tgtframe)
      (neg-adj srcframe tgtframe)
      (pseudo-adjustable srcframe tgtframe)))

(defn add-adj-to
  "Given that source and target are caseframes:
    Adds target to the list of frames source is adjustable to, and
    Adds source to the list of frames target is adjustable from."
  [source target]
  (dosync
    (alter (:adj-to source) conj target)
    (alter (:adj-from target) conj source)))

(defn check-new-caseframe
  "If there is already a caseframe with the given typename
        and slots (order doesn't matter),
    raises an error;
    Otherwise, returns."
  [typename slots]
  (doseq [oldcf @CASEFRAMES
            :let [oldslots (:slots oldcf)
                  numnewslots (count slots)]
            :when (and
                    (= (keyword typename) (:type oldcf))
                    (= (count oldslots) numnewslots)
                    (every? #(some #{(:name %)} slots) oldslots))]
    (error "A caseframe with type = " typename " and slots = " slots " already exists."))
  true)

;Molecular Terms, Caseframes, Caseframe Print-Patterns, Parsing, and
;Generating.
;
;The user types and sees a molecular term in KIF format: (f a1 ... an).
;The issues are:
;    finding the caseframe to use to represent the term (parsing);
;    reconstructing the expression from the stored term (generating).
;
;Generation is easy.
;    The stored term is essentially represented as an instance,:
;        (slot1 filler1 ... slotn fillern)
;    of a caseframe that has a sequence of slots (slot1 ... slotn)
;    and a print-pattern, a list of pattern elements: (pe1 ... pen),
;        where each pei is a slot name, or a quoted symbol.
;    The generated expression is (pe1' ... pen'),
;        where pei' is pei itself if pei is quoted,
;                   and is the generated term(s) which fill the pei
;                   slot otherwise.
;
;Parsing is more difficult.
;It requires retrieving the caseframe from the function symbol, f.
;There are 3 cases:
;1)  The function symbol doesn't occur in the represented term, but is represented by the caseframe.
;    An example is Isa, represented by the (member class) caseframe.
;    In this case the function symbol is mapped to the caseframe in FN2CF,
;                 and appears quoted in the print-pattern.
;
;2) The function symbol occurs in the represented term,
;       because it is one of several instances
;       of a more general function, which is represented by the case frame.
;   Examples are (senseFor smell) and (go left),
;       which might both be represented by the (action object) caseframe.
;   In this case, the print-pattern will have all non-quoted pe's.
;                 and every function symbol that uses this caseframe
;                     needs to be mapped to the caseframe in FN2CF.
;
;3) The function symbol is a molecular term.
;   For example, for any binary relation R,
;       e(R) could be the reflexive closure of R
;       defined by the axiom
;           (forall R (=> (binaryRelation R) (forall (x y) (iff ((e R) x y) (or (R x y) (= x y))))))
;   Generation is easily done:
;       I'll assume that pe1 of the print pattern will be an unquoted slot name,
;       and the function-symbol slot will be filled by a molecular term
;   but for parsing, we need a mapping from e to the caseframe that
;   will be used for representing (e R),
;   and then a mapping from that term to the caseframe that will be
;   used for the representation of ((e R) a b).
;   We can do that by having a caseframe as the key in the mapping FN2CF.
;   Map e to the caseframe for (e R),
;       and that caseframe to the caseframe for ((e R) a b), etc.
;   In general the function symbol slot could be filled by a molecular term,
;      whose function symbol slot is filled by a molecular term, etc.
;      E.g. (((e1 R) a b) c d), and so on.
;      Map e1 to its caseframe, that caseframe to the next, etc.
;   The problems are now:
;      How does the user tell SNePS what that sequence is?
;      How does the user specify which slot gets the function "symbol"?
;   Idea:  The user should use defineCaseframe as for when pe1 is a slot name
;          and the fsymbols argument should include the primitive function symbol
;               enclosed in the appropriate number of lists.
;          So for (senseFor smell) and (go left)
;	   the fsymbols are senseFor and go
;             for ((e R) a b) the fsymbol is (e)
;             for (((e1 R) a b) c d) the fsymbol is ((e1))

(defn define-caseframe
  [typename slots & {:keys [docstring print-pattern fsymbols] :or {docstring ""}}]
  {:pre [(csneps/semantic-type-p (keyword typename))
         (check-new-caseframe typename slots)
         (string? docstring)
         (seq? print-pattern)
         (or (nil? fsymbols) (seq? fsymbols))
         (or (and (seq? (first print-pattern)) 
                   (= (first (first print-pattern)) 'quote))
              fsymbols)]}

  ;;Verify print-pattern
  (map #(when-not (seq? %)
          (when-not (or (some (hash-set %) slots)
                    (some (slot/find-slot %) slots))
            (throw Exception (str "The print-pattern slot " % " is not among the list of slots " slots "."))))
    print-pattern)


  (let [cf (new-caseframe {:type (keyword typename) :docstring docstring
                           :descfun (if (= docstring "") 
                                      (fn [trm] (print-str trm))
                                      (make-description-function docstring))
                           :print-pattern print-pattern
                           :slots (map #(slot/find-slot %) slots)})] ;;Missing error handling.
    (cond
      (and (seq? (first print-pattern)) (= (ffirst print-pattern) 'quote))
        (do
            (let [fname (first (rest (first print-pattern)))] ;;cadar
              (when (and (find-frame fname)
                         (not= (find-frame fname) cf))
                (println fname " being redefined from " (find-frame fname)))
              (dosync (alter FN2CF assoc fname cf)))
          (when fsymbols
            (println "Function symbols " fsymbols " being ignored because the print-pattern
                starts with a quoted symbol.")))
      :else
        (doall (map #(add-fn-cf-map % cf) fsymbols)))
    
   ;;Adjustable stuff:
   (doseq [cf2 (seq @CASEFRAMES)]
     ;; Look at all existing caseframes, check whether they are adjustable to or
		 ;; from this one. If so, store that information in the frames.
     (when-not (= cf cf2)
       (when (adjustable? cf cf2)
         (add-adj-to cf cf2))
       (when (adjustable? cf2 cf)
         (add-adj-to cf2 cf))))
   (dosync (alter CASEFRAMES conj cf))
   cf))

(defn list-caseframes
  "Print all the caseframes."
  []
  (doseq [s @CASEFRAMES]
    (println s)))

(defn add-caseframe-term
  "Adds a term to a caseframe's list of terms that use it.
   If the caseframe cf is given, add the term to that caseframe.
   Else, add the term to the caseframe that term uses."
  [term & {:keys [cf]}]
  (dosync (alter (:terms (if cf cf (@csneps/caseframe term))) conj term)))

(defn quotedpp?
  "Returns True if the caseframe cf
        has a print-pattern with a quoted first symbol;
     False otherwise."
  [cf]
  (and (seq? (first (:print-pattern cf)))
       (= (ffirst (:print-pattern cf)) 'quote)))

(defn sameFrame 
  "Associates the same frame associated with the function symbol oldf
     with the symbol, or list of symbols, newf."
  [newf oldf]
  (assert (or (symbol? newf) (seq? newf)) "newf must be either a symbol or sequence of symbols.")
  (assert (symbol? oldf) "The existing function symbol must be given as a Symbol.")
  (assert (find-frame oldf) (str oldf " does not have a caseframe defined for it."))
  (if (symbol? newf)
    (dosync (alter FN2CF assoc newf (find-frame oldf)))
    (map #(alter FN2CF assoc % (find-frame oldf)) newf))
  nil)

(defn defineNoviceCaseframe
    "Defines a caseframe for the novice user
     for the function named fn, which is known not to already have a caseframe.
     fn is a symbol or a molecular node used as a functional-term function.
     expr is the term the user is trying to build, (fn arg1 arg2 ...)."
  [fn expr]
  (let [numslots (count expr)]
    (if (@NoviceCaseframes numslots)
      ;; Already a novice caseframe for this number of slots
      (dosync (alter FN2CF assoc fn (@NoviceCaseframes numslots)))
      ;; Need to make a new novice caseframe for this number of slots
      (let [slotnames (conj (loop [no 1
                                   names '[]]
                              (if (= no numslots)
                                (seq names)
                                (recur (inc no) (conj names (symbol (str "arg" no))))))
                            'fn)
            slots (map #(or (slot/find-slot % :errorp nil)
                            (slot/define-slot %))
                       slotnames)
            newcf (define-caseframe :Entity slots 
                                    :docstring (pprint/cl-format nil "(~{[~S]~^ ~})" slotnames)
                                    :print-pattern slotnames
                                    :fsymbols (list fn))]
        (dosync (alter NoviceCaseframes assoc numslots newcf))))
    (@NoviceCaseframes numslots)))

(defn dcsRelationTermsetMap
  "Given a term, returns a map with the keys being relations and the values being sets of terms for each
   item in the original terms down cableset."
  [term]
  (apply merge (map hash-map (:slots (@csneps/caseframe term)) (@csneps/down-cableset term))))
  
(defn hasOneArgumentSlot
  [cf]
  (= (- (count (:slots cf))
        (if (quotedpp? cf) 0 1))
     1))
