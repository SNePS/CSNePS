;;; CSNePS: Slot-Based Inference
;;; =============
;;; Written by Jonathan P. Bona
;;; Ported to Clojure by Daniel R. Schlegel
;;;    
;;; The contents of this file are subject to the University at Buffalo
;;; Public License Version 1.0 (the "License"); you may not use this file
;;; except in compliance with the License. You may obtain a copy of the
;;; License at http://www.cse.buffalo.edu/sneps/Downloads/ubpl.pdf.
;;; 
;;; Software distributed under the License is distributed on an "AS IS"
;;; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
;;; the License for the specific language governing rights and limitations
;;; under the License.
;;; 
;;; The Original Code is CSNePS.
;;; 
;;; The Initial Developer of the Original Code is Research Foundation of
;;; State University of New York, on behalf of University at Buffalo.
;;; 
;;; Portions created by the Initial Developer are Copyright (C) 2012
;;; Research Foundation of State University of New York, on behalf of
;;; University at Buffalo. All Rights Reserved.
;;; 
;;; Contributor(s): ______________________________________.

(in-ns 'csneps.snip)

(declare covers valid-adjust)

(defmulti slot-based-entails
  (fn [source target] [(csneps/syntactic-type-of source) (csneps/syntactic-type-of target)]))

;;; Slot-based entailment never applies to an atom and anything
(defmethod slot-based-entails [:csneps.core/Atom :csneps.core/Molecular] [source target])
(defmethod slot-based-entails [:csneps.core/Molecular :csneps.core/Atom] [source target])
(defmethod slot-based-entails [:csneps.core/Atom :csneps.core/Atom] [source target])

(defmethod slot-based-entails 
  [:csneps.core/Negation :csneps.core/Nand] [source target]
  ;; nor -> nand
  ;; (nor (and P Q) R) |= (nand P Q)
  ;; if any of the source's fillers is a conjunction X s.t. 
  ;; X's fillers are a subset of the target's fillers, 
  ;; then the source entails the target so return it.
  (loop
    [args (find-utils/findto source 'nor)]
    (let [arg (first args)]
      (cond 
        (and (= (csneps/syntactic-type-of arg) :csneps.core/Conjunction)
               (subset? (find-utils/findto arg 'and) (find-utils/findto target 'andorargs)))
        target
        (nil? (rest args))
        nil
        :else
        (recur (rest args))))))

(defmethod slot-based-entails 
  [:csneps.core/Implication :csneps.core/Implication] [source target]
  (let [src-ant (nth (:down-cableset source) 0)
        src-cq  (nth (:down-cableset target) 1)
        tgt-ant (nth (:down-cableset source) 0)
        tgt-cq  (nth (:down-cableset target) 1)]
    (when (and (subset? src-ant tgt-ant) (subset? tgt-cq src-cq))
      target)))

(defmethod slot-based-entails 
  [:csneps.core/Andor :csneps.core/Andor] [source target]
  (let [i       (:min source)
        j       (:max source)
        src-set (nth (:down-cableset source) 0)
        tgt-set (nth (:down-cableset target) 0)
        k       (- (count src-set) (count tgt-set))]
    (when (or 
            (and (>= k 0)
                 (subset? tgt-set src-set)
                 (= (max (- i k) 0) (:min target))
                 (= (min j (count tgt-set)) (:max target)))
            (and (subset? src-set tgt-set)
                 (= i (:min target))
                 (= (- j k) (:max target))))
      target)))

(defmethod slot-based-entails 
  [:csneps.core/Thresh :csneps.core/Thresh] [source target]
  (let [i       (:min source)
        j       (:max source)
        src-set (nth (:down-cableset source) 0)
        tgt-set (nth (:down-cableset target) 0)
        k       (- (count src-set) (count tgt-set))]
    (when (or 
            (and (>= k 0)
                 (subset? tgt-set src-set)
                 (= (min i (count tgt-set)) (:min target))
                 (= (max (- j k) i) (:max target)))
            (and (subset? src-set tgt-set)
                 (= (- i k) (:min target))
                 (= j (:max target))))
      target)))

(defmethod slot-based-entails 
  [:csneps.core/Negation :csneps.core/Negation] [negsource negtarget]
  (when (not= negsource negtarget)
    (let [sourceset (find-utils/findto negsource 'nor)
          targetset (find-utils/findto negtarget 'nor)]
      ;; If the source and target sets are singletons, perform normal slot-based inference
      (if (and (= (count sourceset) 1) (= (count targetset) 1))
        (let [source (first sourceset)
              target (first targetset)]
          ;; If source and target are molecular, and have compatible frames
	        ;; TODO:     should this "adjustable" check be eliminated?
          (if (and (isa? (csneps/syntactic-type-of target) :csneps.core/Molecular)
                   (isa? (csneps/syntactic-type-of source) :csneps.core/Molecular)
                   (cf/adjustable? (:caseframe source) (:caseframe target)))
            ;; Return targetset if the fillers of every source slot can 
		        ;;     be validly adjusted to the fillers of the corresponding target slot
            (when (every?
                    #(valid-adjust (:negadjust %)
                                   (:min %)
                                   (:max %)
                                   (pb-findtos (hash-set source) %)
                                   (pb-findtos (hash-set target) %))
                    (:slots (:caseframe source)))
              targetset)))
        ;; Else (not singletons;  special nor case): 
	      ;;      return targetset if the source set covers the target set
        (when (covers sourceset targetset) targetset)))))

(defmethod slot-based-entails 
  [:csneps.core/Molecular :csneps.core/Molecular] [source target]
  ;(println "Here" (cf/adjustable? (:caseframe source) (:caseframe target)))
  (when (and (cf/adjustable? (:caseframe source) (:caseframe target))
             (every? 
               #(valid-adjust (:posadjust %)
                              (:min %)
                              (:max %)
                              (pb-findtos (hash-set source) %)
                              (pb-findtos (hash-set target) %))
               (:slots (:caseframe source))))
    target))

(defn covers
  "Returns true if target and source are sets of propositions
        such that every proposition in target is eq to,
             or is slot-based-entailed by some proposition in source"
  [source target]
  (every? (fn [tgt]
            (some 
              (fn [src] (or (= src tgt)
                            (slot-based-entails src tgt)))
              source))
          target))

;; TODO: I used case here. ecase was used in the CL impl. Check on effects.
(defn valid-adjust
  "Returns t if the sourcefillers can be adjusted
        via the adjust type adj to the targetfillers"
  [adj min max sourcefillers targetfillers]
  (when-not (or (empty? sourcefillers) (empty? targetfillers))
    (and (<= min (count targetfillers))
         (or (nil? max) (<= (count targetfillers) max))
         (case adj
           reduce (or (= targetfillers sourcefillers)
                       (subset? (set targetfillers) (set sourcefillers)))
           expand (or (= sourcefillers targetfillers)
                       (subset? (set sourcefillers) (set targetfillers)))
           none   (= sourcefillers targetfillers)))))

(defn sb-derivable-test
  "Returns t if target is slot-based-entailed by term 
       and if term is asserted in context;
     Else, returns nil."
  [term target context termstack]
  (and
    (not= term target)
    (not-any? (hash-set term) termstack)
    (slot-based-entails term target)
    (seq (askif term context (cons target termstack)))))

(defn slot-based-derivable
  "If the term [target] is entailed in the given context
          via slot-based-inference
          assert it,
          and return a set containing it;
     else return the empty set.
     The termstack is a stack of propositions
         that this goal is a subgoal of."
  [target context termstack]
  (if (isa? (csneps/type-of target) :csneps.core/Molecular) 
    (do 
	    ;;Look at the terms stored in the target's caseframe
	    (when GOALTRACE 
	      (cl-format true "~&I will consider using Slot&Path-Based inference.~%"))
	    (or 
	      (loop 
	        [terms @(:terms (:caseframe target))]
	        (cond 
	          (empty? terms) 
	          nil
	          (sb-derivable-test (first terms) target context termstack)
	          (do 
	            (assertTrace (first terms) nil target "Slot-Based inference" context)
	            (hash-set (list target)))
	          :else (recur (rest terms))))
	      ;; For each caseframe cf that can be adjusted to the target's frame, 
	      ;;  look at each of cf's stored terms
	      (loop 
	        [cfs @(:adj-from (:caseframe target))]
	        (let [cf (first cfs)
	              res (when cf
	                    (loop 
	                      [terms @(:terms cf)]
	                      (cond 
                         (empty? terms) 
                         nil
                         (sb-derivable-test (first terms) target context termstack)
                         (do 
                           (assertTrace (first terms) nil target "Slot-Based inference" context)
                           (hash-set (list target)))
                         :else (recur (rest terms)))))]
	          (when-not (nil? cf)
	            (recur (rest cfs)))))
	      (hash-set)))
    (hash-set)))