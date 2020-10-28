;; Convert (part of) a CSNePS KB to a dot graph to be rendered with Graphviz.

(ns csneps.utils.dotgraph
  (:require [csneps.core :as csneps]
            [csneps.core.caseframes :as cf]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn filler-to-dot [term slot filler]
  (str (:name term) " -> " (:name filler) " [label=\"  " (:name slot) "\"];"))

(defn relation-to-dot [term slot fillers]
  (str/join "\n" (map #(filler-to-dot term slot %) fillers)))

(defn relations-to-dot [term term-relations]
  (str/join "\n"
            (for [[slot fillers] term-relations]
              (relation-to-dot term slot fillers))))

(defn terms-to-dot [terms]
  (str/join "\n"
            (for [term terms
                  :let [term-relations (cf/dcsRelationTermsetMap term)]]
              (relations-to-dot term term-relations))))

(defn generate-dotfile [terms fname]
  (let [termstr (terms-to-dot terms)]
    (with-open [w ^java.io.Writer (io/writer fname)]
      (.write w "digraph G {\n")
      (.write w ^String termstr)
      (.write w "\n}"))))
