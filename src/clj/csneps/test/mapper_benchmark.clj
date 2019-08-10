(ns csneps.test.mapper-benchmark
  (:require [csneps.core.contexts :as ct]
            [csneps.core.caseframes :as cf]
            [csneps.core.printer :as print]
            [csneps.core.relations :as slot]
            [csneps.core :as csneps]
            [csneps.core.semantic-types :as st]
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
  '[properNounToName1
    properNounToName2
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
  [msgfile framefile rulefile]
  (load-file framefile)
  (sneps3kbtocsneps msgfile)
  (semtypesToObjLang)
  (load-file rulefile))

(defn synsem-one-file
  [msgfile framefile rulefile]
  (snuser/clearkb true)
  (loadkb msgfile framefile rulefile)
  (let [start-time (. java.lang.System (clojure.core/nanoTime))]
    (snuser/adopt-rules adopt-order)
    (log-elapsed start-time)
    (print-time)))

(defn synsem-benchmark
  [msgfolder framefile rulefile]
  (reset-benchmark)
  (doseq [f (file-seq (clojure.java.io/file msgfolder))]
    (when-not (.isDirectory f)
      (synsem-one-file (.getPath f) framefile rulefile))))


;;; Util fns

(defn semtypesToObjLang
  []
;  (doseq [[c ps] (:parents @csneps/semantic-type-hierarchy)
;          p ps]
;    (snuser/assert `(~'Isa (~'every ~'x (~'Isa ~'x ~(name c))) ~(name p))))
  (let [terms (filter csneps/atomicTerm? (vals @csneps/TERMS))]
    (doseq [t terms]
      (snuser/assert ['Isa t (name (st/semantic-type-of t))]))))

(defn typeToGeneric
  [typestr]
  (let [typeseq (read-string typestr)]
    (str (list 'Isa (list 'every 'x (list 'Isa 'x (second typeseq))) (nth typeseq 2)))))

(defn sneps3kbtocsneps
  [filename]
  (let [filestr (-> (slurp filename)
                  (str/replace "ct:assert" "csneps.core.snuser/assert")
                  (str/replace " 'DefaultCT :origintag :hyp" "")
                  (str/replace "|" "\"")
                  (str/replace "\"\"\"" "\"\\\"\"")
                  (str/replace "(load" "(comment")
                  (str/replace #"\(csneps.core.snuser/assert '\(Message.*?\)\)" "") ;; Not using the message assertion, lets ignore it since it has weird parsing requirements.
                  (str/replace #"\(csneps.core.snuser/assert '\(SyntacticCategoryOf POS.*?\)\)" "")
                  (str/replace "(in-package :snuser)" "(in-ns 'csneos.core.snuser)")
                  (str/replace "Action" "Action1"))
        typestrings (re-seq #"\(Type\s\S+\s\S+?\)" filestr)
        filestr (loop [typestrings typestrings
                      fs filestr]
                 (if (seq typestrings)
                  (recur (rest typestrings)
                        (str/replace fs (first typestrings) (typeToGeneric (first typestrings))))
                  fs))
        mgrsstrings (set (re-seq #"\d+[A-Z]+\d+" filestr))
        filestr (loop [mgrsstrings mgrsstrings
                       fs filestr]
                  (if (seq mgrsstrings)
                    (recur (rest mgrsstrings)
                           (str/replace fs (first mgrsstrings) (str \" (first mgrsstrings) \")))
                    fs))]
    (println filestr)
    (load-string filestr)))
    

  
  