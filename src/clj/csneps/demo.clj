(ns csneps.demo
  (:use [clojure.java.io :only (reader)]
        [clojure.pprint :only (cl-format pprint)]))

(defn noop
  [])

(defn demo 
  "Echoes and evaluates the forms in the file.
   If pause is true, will pause after echoing each form,
   but before evaluating it. If failonerror is true, an
   exception will halt the demo.
     If the file is omitted, a menu will be presented of available demos."
  [& {:keys [file pause failonerror] :or {file nil, pause nil, failonerror nil}}]
  (when file
    (with-open [r (java.io.PushbackReader.
                  (clojure.java.io/reader file))]
      (binding [*read-eval* false]
        (loop [form (read r false nil)
               keep-pausing (ref pause)
               continue (ref true)]
          (when (and form @continue)
            (println "\ninput: " form)
            (when @keep-pausing
              (loop []
                (println "\n--- pause ---\n")
                (let [usrinput (read-line)]
                  (case usrinput
                    "" (noop)
                    "c" (dosync (ref-set keep-pausing nil))
                    "q" (dosync (ref-set continue nil))
                    ("l" "^") (do 
                                (println "Demo interrupted. Type exit and press enter to continue.")
                                (clojure.main/repl :read (fn [request-prompt request-exit]
                                                           (let [form (clojure.main/repl-read request-prompt request-exit)]
                                                             (if (= 'exit form) request-exit form))))
                                (recur))
                    ("?" "h") (do 
                                (cl-format true
                                           "~%The following commands are available at pause points:~
                                            ~%  h,?            Print this help message~
                                            ~%  l,^            Enter Lisp read/eval/print loop~
                                            ~%  c              Continue without pausing~
                                            ~%  q              Quit the demo~
                                            ~%  RETURN         Continue the demo~
                                            ~%")
                                (recur))
                    (recur))))))
          (when (and form @continue)
            (if failonerror
              (println "output:" (eval form))
              (println "output:" (try (eval form) (catch Exception e (.getMessage e))))) ;; We watch to catch errors, print them, and move on sometimes.
            (recur (read r false nil) keep-pausing continue)))))))