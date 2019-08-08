(ns csneps.configuration)

;; Inference Graph Configuration
(def ig-cpus-to-use (/ (.availableProcessors (Runtime/getRuntime)) 2))
;(def ig-cpus-to-use 1)

;; Experimental support for using the object language for semantic types.
(def semtype-objectlang-experimental false)