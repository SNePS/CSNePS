(ns csneps.core.unify.treenode
  (:require [csneps.core :as csneps])
  (:use [csneps.util]
        [csneps.utils.coreutils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unification Tree Structure ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Map containing the distribution nodes for the tree.
(def DistNodes (ref {}))

;; A distribution node is made up of the predicate name and arity which it
;; representes, and the children in the tree.
(defrecord2 DistNode
            [name ""
             arity 0
             children (ref {})])

(defrecord2 UnifNode
            [acceptString ""      ;; The string for the item to be matched to.
             acceptWft nil
             acceptArity nil      ;; The arity of the item matching to.
             acceptDepth 0        ;; Depth of the current node in the expression.
             children (ref {})
             parent nil])         ;; Important for re-building sub-sexpressions...

;; TODO: Remove this once all references are gone.
;(defrecord2 TerminalNode
;  [sexpr ""
;   wft nil
;   parent nil])

(defn distnode? [node]
  (= (type node) csneps.core.unify.treenode.DistNode))

(defn atomwftnode? [node]
  (zero? (:acceptArity node)))

(defn molwftnode? [node]
  (csneps/molecularTerm? (:acceptWft node)))

(defn labelnode? [node]
  (nil? (:acceptWft node)))

(defn- collectWftNodes
  [currnode & {:keys [depth] :or {depth 0}}]
  ;(println "Collecting! " depth (:acceptString currnode) currnode)
  (cond
    (and (= depth 1) (molwftnode? currnode))  currnode
    (molwftnode? currnode)                    (map #(collectWftNodes % :depth (dec depth)) (vals @(:children currnode)))
    (labelnode? currnode)                     (map #(collectWftNodes % :depth (inc depth)) (vals @(:children currnode)))
    :else                                     (map #(collectWftNodes % :depth depth) (vals @(:children currnode)))))

(defn getWftNodesFor
  [currnode]
  ;(println "----")
  (if (atomwftnode? currnode)
    (list currnode)
    (flatten (collectWftNodes currnode))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Building the tree ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn findDistNode
  "Determines if a distribution node for the given name and arity already exist.
    If such a node exists, returns it, otherwise returns nil."
  [name arity distnodes]
  (get distnodes (str name arity)))

(defn findUnifNode
  "Determines if a unification node for term name of a given arity arleady exist
    as a child as the parent node. If so, returns it, otherwise returns nil."
  [name arity parent]
  (get @(:children parent) (str name arity)))

(defn buildUnificationTreeNode
  "Builds a unification tree node. If a parent node exists, the node is added
    as a child of the parent (if it isn't already). If there is no parent, a new
    distribution node is created (if one doesn't already exist) and returned."
  [termid arity & {:keys [parent wft distnodes] :or {distnodes DistNodes}}]
  (dosync
    (if parent
      (let [node (or (findUnifNode termid arity parent)
                     (new-unif-node {:acceptString termid :acceptArity arity :acceptWft wft :parent parent}))]
        (alter (:children parent) assoc (str termid arity) node)
        node)
      (let [node (or (findDistNode termid arity @distnodes)
                     (new-dist-node {:name termid :arity arity}))]
        (alter distnodes assoc (str termid arity) node)
        node))))

(defn addTermToUnificationTree
  [term & {:keys [parent distnodes] :or {distnodes DistNodes}}]
  (if (isa? (csneps/syntactic-type-of term) :csneps.core/Molecular)
    (let [termid (term-predicate term)
          termpp (:print-pattern (csneps/caseframe-for term))
          nofsyms (and (seq? (first termpp)) (= (ffirst termpp) 'quote))
          arity (dec (count (:print-pattern (csneps/caseframe-for term))))
          newparent (buildUnificationTreeNode termid arity :parent parent :distnodes distnodes)] ; ;Builds the node for the predicate symbol
      (loop [p newparent
             dcs (if nofsyms
                   (@csneps/down-cableset term)
                   (rest (@csneps/down-cableset term)))]
        ;(binding [*print-level* 4]
        ;  (println p "\n" dcs))
        (if (= dcs '())
          (buildUnificationTreeNode termid arity :parent p :wft term :distnodes distnodes) ;; Builds the node for the wft
          (recur (addTermToUnificationTree
                   (if (= 1 (count (first dcs))) ;;Singleton sets shouldn't be added as sets.
                     (ffirst dcs)
                     (first dcs))
                   :parent p)
                 (rest dcs)))))
    (let [termid (or (:name term) (str term)) ;;Handling sets.
          arity 0
          newparent (buildUnificationTreeNode termid arity :parent parent :wft term :distnodes distnodes)]
      newparent)))