;; Convert (part of) a CSNePS KB to a dot graph to be rendered with Graphviz.

(ns csneps.utils.dotgraph
  (:require [csneps.core :as csneps]
            [csneps.core.semantic-types :as st]
            [csneps.core.caseframes :as cf]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn filler-to-dot [term slot filler]
  (str (:name term) " -> " (:name filler) " [label=\"  " (:name slot) "\" arrowhead=\"open\"];"))

(defn relation-to-dot [term slot fillers]
  (str/join "\n" (map #(filler-to-dot term slot %) fillers)))

;; Pre-supposes collapseable.
(defn collapsed-relations-to-dot [term term-relations]
  (let [cf (csneps/caseframe-for term)
        slots (:slots cf)
        origin (if (= (count slots) 2)
                 (first slots) ;; 2 slot case
                 (second slots)) ;; 3 slot case
        destination (if (= (count slots) 2)
                      (second slots) ;; 2 slot case
                      (nth slots 2))] ;; 3 slot case
    (str (:name (first (term-relations origin)))
         " -> "
         (:name (first (term-relations destination)))
         " [label=\"  " (cf/caseframe-name cf) "\" arrowhead=\"empty\"];")))

;; There are two cases for collapseable terms:
;; 3 slots, Propositional, has fsymbols, no in edges.
;; 2 slots, Propositional, no in edges.
(defn collapseable? [term]
  (let [cf (csneps/caseframe-for term)]
    (and (empty? (@csneps/up-cablesetw term))
         (csneps/subtypep (st/semantic-type-of term) :Propositional)
         (or
           (and (= 2 (count (:slots cf)))
                (cf/quotedpp? cf)) ;; Quoted if no fsymbols.
           (and (= 3 (count (:slots cf)))
                (not (cf/quotedpp? cf)))))))

(defn relations-to-dot [term term-relations collapse?]
  (str/join "\n"
            (for [[slot fillers] term-relations]
                (relation-to-dot term slot fillers))))

(defn terms-to-dot [terms collapse?]
  (str/join "\n"
            (for [term terms
                  :let [term (csneps/get-term term)
                        term-relations (cf/dcsRelationTermsetMap term)]]
              (if (and collapse? (collapseable? term))
                (collapsed-relations-to-dot term term-relations)
                (relations-to-dot term term-relations collapse?)))))

(defn generate-dotfile [terms fname collapse?]
  (let [termstr (terms-to-dot terms collapse?)]
    (with-open [w ^java.io.Writer (io/writer fname)]
      (.write w "digraph G {\n")
      (.write w ^String termstr)
      (.write w "\n}"))))
