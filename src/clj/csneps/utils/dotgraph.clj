;; Convert (part of) a CSNePS KB to a dot graph to be rendered with Graphviz.

(ns csneps.utils.dotgraph
  (:use [clojure.set])
  (:require [csneps.core :as csneps]
            [csneps.core.semantic-types :as st]
            [csneps.core.caseframes :as cf]
            [csneps.core.contexts :as ct]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn display-name [term]
  (if (ct/asserted? term (ct/currentContext))
    (str \" (:name term) "!\"")
    (str \" (:name term) "\"")))

(defn filler-to-dot [term slot filler]
  (str (display-name term) " -> " (display-name filler) " [label=\"  " (:name slot) "\" arrowhead=\"open\"];"))

(defn relation-to-dot [term slot fillers]
  (str/join "\n" (map #(filler-to-dot term slot %) fillers)))

(defn restriction-to-dot [quantterm restriction]
  (str (display-name quantterm) " -> " (display-name restriction) " [label=\"restriction\" style=\"dashed\""))

(defn restrictions-to-dot [quantterm restrictions]
  (str/join "\n" (map #(restriction-to-dot quantterm %) restrictions)))

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
    (str (display-name (first (term-relations origin)))
         " -> "
         (display-name (first (term-relations destination)))
         " [label=\"  " (cf/caseframe-name cf) (if (ct/asserted? term (ct/currentContext)) "!" "")
            "\" arrowhead=\"empty\"];")))

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

(defn relations-to-dot [term term-relations]
  (str/join "\n"
            (for [[slot fillers] term-relations]
                (relation-to-dot term slot fillers))))

;; Possible candidate to replace flatten-term - it's certainly nicer to look at. Performance?
(defn terms-in-term [term]
  (loop [terms #{}
         to-check [term]]
    (if (subset? (set to-check) terms) ;; Should we just check for empty?
      terms
      (recur (conj terms (first to-check))
             (concat (rest to-check)
                     (apply union (@csneps/down-cableset (first to-check)))
                     (@csneps/restriction-set (first to-check)))))))

(defn terms-to-dot [terms collapse?]
  (let [terms (map csneps/get-term terms)]
    (str/join "\n"
              (for [term (apply union (map terms-in-term terms)) ;; Get all subterms so we don't omit anything!
                    :let [term-relations (cf/dcsRelationTermsetMap term)]]
                (if (and collapse? (collapseable? term))
                  (collapsed-relations-to-dot term term-relations)
                  (relations-to-dot term term-relations))))))

(defn generate-dotfile [terms fname collapse?]
  (let [termstr (terms-to-dot terms collapse?)]
    (with-open [w ^java.io.Writer (io/writer fname)]
      (.write w "digraph G {\n")
      (.write w ^String termstr)
      (.write w "\n}"))))