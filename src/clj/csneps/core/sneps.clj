(ns sneps

)

;; Load the util namespace
(load "util")
;; Load the sneps3-substitution namespace
(load "sneps3_substitution")
;; Load the namespace contexts
(load "contexts")
;; Load the namespace sneps3
(load "sneps3")
;; Load the 'relations' namespace
(load "relations")
;; Load the 'find' namespace
;(load "find")
;; Load the 'caseframes' namespace
(load "caseframes")
;; Load the sneps3-printer namespace
(load "sneps3_printer")
;; Load the contexts namespace
(load "contexts")
;; Load the build namespace
(load "sneps3/core/build")
;; Load the arithmetic namespace.
(load "arithmetic")
;; Load the snip namespace.
(load "snip/snip")
;; Load the snuser namespace.
(load "snuser")
;; Initialize the system.
;(load "initialize")

(snuser/clearkb true)

;(clojure.main/repl :print sneps3/sneps-printer) ;;Belongs in the clojure init script.