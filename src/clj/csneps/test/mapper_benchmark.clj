(ns csneps.test.mapper-benchmark
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.printer :as print]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.build :as build]
            [csneps.snip :as snip]
            [clojure.set :as set]
            [clojure.string :as str]
            [csneps.core.snuser :as snuser]))

(declare sneps3kbtocsneps semtypesToObjLang)

(def runtime (atom 0))

(defn log-elapsed
  [start-time]
  (swap! runtime + (/ (- (. java.lang.System (clojure.core/nanoTime)) start-time) 1000000.0)))

(defn reset-benchmark
  []
  (def runtime (atom 0)))

(defn print-time
  []
  (println "Run Time: " @runtime))

(def adopt-order
  '[[generalizeNN generalizeNNP generalizeNNPS generalizeNNS generalizeNP generalizeNPS]
    [generalizeVBD generalizeVBG generalizeVBN generalizeVBP generalizeVB generalizeVBZ]
    properNounToName
    organizationHasName
    nnName
    nounPhraseToInstance
    eventToInstance
    pluralNounToGroup
    subjAction
    dobjAction
    prepToRelation
    nnToModifier
    amodToModifier])

(defn loadkb
  [msgfile rulefile]
  (load-file rulefile)
  (sneps3kbtocsneps msgfile)
  (semtypesToObjLang))

(defn synsem-one-file
  [msgfile rulefile]
  (snuser/clearkb true)
  (loadkb msgfile rulefile)
  (let [start-time (. java.lang.System (clojure.core/nanoTime))]
    (snuser/adopt-rules adopt-order)
    (log-elapsed start-time)
    (print-time)))

(defn synsem-benchmark
  [msgfolder rulefile]
  (reset-benchmark)
  (doseq [f (file-seq (clojure.java.io/file msgfolder))]
    (synsem-one-file (.getPath f) rulefile)))


;;; Util fns

(defn semtypesToObjLang
  []
  (doseq [[c ps] (:parents @csneps/semantic-type-hierarchy)
          p ps]
    (snuser/assert `(~'Isa (~'every ~'x (~'Isa ~'x ~(name c))) ~(name p))))
  (let [terms (filter #(not (isa? @csneps/semantic-type-hierarchy (csneps/semantic-type-of %) :Propositional)) (vals @csneps/TERMS))]
    (doseq [t terms]
      (snuser/assert ['Isa t (name (csneps/semantic-type-of t))]))))

(defn sneps3kbtocsneps
  [filename]
  (let [filestr (-> (slurp filename)
                  (str/replace "ct:assert" "csneps.core.snuser/assert")
                  (str/replace " 'DefaultCT :origintag :hyp" "")
                  (str/replace "|" "\"")
                  (str/replace "(load" "(comment")
                  (str/replace "(in-package :snuser)" "(in-ns 'csneos.core.snuser)"))
        mgrsstrings (re-seq #"\d+[A-Z]+\d+" filestr)
        filestr (loop [mgrsstrings mgrsstrings
                       fs filestr]
                  (if (seq mgrsstrings)
                    (recur (rest mgrsstrings)
                           (str/replace fs (first mgrsstrings) (str \" (first mgrsstrings) \")))
                    fs))]
    (load-string filestr)))
    

  
  