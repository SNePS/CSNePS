(ns csneps.gui
  (:import [csneps.gui GUI2])
  (:use [clojure.tools.nrepl.server :only (start-server stop-server)]
        [csneps.util])
  (:require [clojure.tools.nrepl :as repl]
            [csneps.core :as csneps]
            [clojure.set :as set]
            [csneps.core.relations :as slot]
            [csneps.core.caseframes :as cf]
            [csneps.core.contexts :as ct]))

(def gui (ref nil))

(defn add-watches
  [model]
  (let [typechangefn (fn [ref key oldvalue newvalue] (.typesChanged model
                                                                    (map-difference (:parents oldvalue) (:parents newvalue))
                                                                    (if (>= (count oldvalue) (count newvalue)) true false)))
        termchangefn (fn [ref key oldvalue newvalue] (.termsChanged model
                                                                    (map-difference oldvalue newvalue)
                                                                    (empty? newvalue)))
        contextchangefn (fn [ref key oldvalue newvalue] (let [clear? (>= (count oldvalue) (count newvalue))]
                                                          (if clear?
                                                            (.contextsChanged
                                                              model
                                                              newvalue
                                                              true)
                                                            (.contextsChanged
                                                              model(GUI2/getModel)
                                                              (map-difference oldvalue newvalue)
                                                              false))))
        slotchangefn (fn [ref key oldvalue newvalue] (.slotsChanged model
                                                                    (map-difference oldvalue newvalue)
                                                                    (if (>= (count oldvalue) (count newvalue)) true false)))
        caseframechangefn (fn [ref key oldvalue newvalue] (.caseframesChanged model
                                                                              (set/difference newvalue oldvalue)
                                                                              (if (>= (count oldvalue) (count newvalue)) true false)))
        fsymbolchangefn (fn [ref key oldvalue newvalue] (.fsymbolsChanged model
                                                                          (map-difference oldvalue newvalue)))
        currentcontexthypschangefn (fn [ref key oldvalue newvalue] (.currentContextHypsChanged model
                                                                                               (set/difference newvalue oldvalue)))
        currentcontextchangefn (fn [ref key oldvalue newvalue] (.currentContextChanged model
                                                                                       newvalue)
                                 (remove-watch (:hyps oldvalue) :currhyps)
                                 (add-watch (:hyps newvalue) :currhyps currentcontexthypschangefn))
        ;; Refs removed from terms.
        ichannelschangefn (fn [ref key oldvalue newvalue]
                            (.termNameIChannelMapChanged model
                                                         (map-difference newvalue oldvalue)
                                                         (empty? newvalue)))
        uchannelschangefn (fn [ref key oldvalue newvalue]
                            (.termNameUChannelMapChanged model
                                                         (map-difference newvalue oldvalue)
                                                         (empty? newvalue)))
        gchannelschangefn (fn [ref key oldvalue newvalue]
                            (.termNameGChannelMapChanged model
                                                         (map-difference newvalue oldvalue)
                                                         (empty? newvalue)))]
    (add-watch csneps/semantic-type-hierarchy :types typechangefn)
    (add-watch csneps/TERMS :terms termchangefn)
    (add-watch ct/CONTEXTS :contexts contextchangefn)
    (add-watch slot/SLOTS :slots slotchangefn)
    (add-watch cf/CASEFRAMES :cfs caseframechangefn)
    (add-watch cf/FN2CF :fsyms fsymbolchangefn)
    (add-watch ct/CONTEXTS :cts contextchangefn)
    (add-watch ct/*CurrentContext* :currct currentcontextchangefn)
    (add-watch (:hyps (ct/currentContext)) :currhyps currentcontexthypschangefn)
    (add-watch csneps/i-channels :ichannels ichannelschangefn)
    (add-watch csneps/g-channels :gchannels gchannelschangefn)
    (add-watch csneps/u-channels :uchannels uchannelschangefn)))

(defn startGUI
  ([] (startGUI (set (vals @csneps/TERMS))))
  ([termset]
    (let [rg (defonce rgen (java.util.Random. 123945))
          port (+ 1000 (.nextInt ^java.util.Random rgen 9000))
          srv (start-server :port port)
          termset (set (map #(csneps/get-term %) termset))
          GUI (new GUI2 port termset false)]
      (dosync (ref-set gui GUI))
      (add-watches GUI2/model))))

;; A clearkb required entirely reinitializing the model.
(defn remove-watches []
  (when @gui
    (remove-watch csneps/semantic-type-hierarchy :types)
    (remove-watch csneps/TERMS :terms)
    (remove-watch ct/CONTEXTS :contexts)
    (remove-watch slot/SLOTS :slots)
    (remove-watch cf/CASEFRAMES :cfs)
    (remove-watch cf/FN2CF :fsyms)
    (remove-watch ct/CONTEXTS :cts)
    (remove-watch ct/*CurrentContext* :currct)
    (remove-watch (:hyps (ct/currentContext)) :currhyps)
    (remove-watch csneps/i-channels :ichannels)
    (remove-watch csneps/g-channels :gchannels)
    (remove-watch csneps/u-channels :uchannels)))

(defn reinitialize-gui []
  (when @gui
    (add-watches GUI2/model)
    (GUI2/initializeModel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for the REPL ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following are heavily modified adaptations of code from CounterClockwise.

;(defn log
;  [^StyledText log ^String s type]
;  (ui-sync
;    (let [charcnt (.getCharCount log)
;          [log-style line-background-color-name] (get log-styles type [default-log-style nil])
;          linecnt (.getLineCount log)]
;      (.append log s)
;      (when-not (re-find #"(\n|\r)$" s) (.append log "\n"))
;      (doto log
;        cursor-at-end
;        .showSelection
;        (.setStyleRange (log-style charcnt (- (.getCharCount log) charcnt))))
;      (when line-background-color-name
;        (.setLineBackground log (dec linecnt) (- (.getLineCount log) linecnt)
;          (-> (CCWPlugin/getDefault) .getColorCache (.get line-background-color-name)))))))

(defn log
  [repl-view s type]
  ;(println "Logging: " (first s))
  (.log ^csneps.gui.business.repl.IREPLView repl-view (str s "\n"))
  ;(.append log-comp s)
  s)

(defn eval-failure-msg
  [status s]
  (format "Expression %s: %s"
    ({"timeout" "timed out", "interrupted" "was interrupted"} status "failed")
    (some-> ^String s
      (.substring 0 (min 30 (count s)))
      (str (when (> (count s) 30) "..."))
      (.replaceAll "\\n|\\r" " "))))

(defn eval-expression
  [repl-view log-component client expr]
  ;(println "Evalulating: " expr " " client)
  ;(println (repl/message client {:op :eval :code "(+ 1 2)" :ns "snuser"}))
  (try
    (repl/message client
                  (if (map? expr)
                    expr
                    {:op :eval :code expr :ns (.getCurrentNamespace ^csneps.gui.business.repl.IREPLView repl-view)}))
    (catch Throwable t
      (.printStackTrace t)
      (log repl-view (eval-failure-msg nil expr) nil))))
      ;(log log-component (eval-failure-msg nil expr)))))
      ;(CCWPlugin/logError (eval-failure-msg nil expr) t)
      ;(log log-component (eval-failure-msg nil expr) :err))))

(defn handle-responses
  [repl-view log-component expr responses]
  (future (doseq [{:keys [out err value ns status] :as resp} responses]
            ;(println "New data: " resp)
            ;(println "Resp: " responses)
            (when ns (.setCurrentNamespace ^csneps.gui.business.repl.IREPLView repl-view ns))
            (doseq [[k v] (dissoc resp :id :ns :status :session)]
              ;(log repl-view v k))
              ;(doseq [[k v] (dissoc resp :id :ns :status :session)]
                (log repl-view v k)
                ;(if (log-styles k)
                ;  (log repl-view log-component v k)
                ;  (CCWPlugin/log (str "Cannot handle REPL response: " k (pr-str resp)))))
              (doseq [status status]
                (case status
                  "interrupted" (log log-component (eval-failure-msg status expr))
                  ;"need-input" (ui-sync (.getStdIn repl-view))
                  nil))
              )
    ;(println "Future terminating")
            )))
      
(defn configure-repl
  [^csneps.gui.business.repl.REPLView repl-view ^javax.swing.JTextArea log-comp repl-client session-id]
    
  (let [session-client (repl/client-session repl-client :session session-id)
        responses-promise (promise)]
  
  (handle-responses repl-view log-comp nil (session-client {:op "describe"}))
  
  ;(session-client {:op :eval :code "(+ 1 2)" :ns "snuser"})
  
  ;;comp applies fns right to left.
  (comp (partial eval-expression repl-view log-comp session-client)
      (fn [expr add-to-log?]
        ;(reset! retained-input nil)
        ;(reset! current-step -1)
        ;(when add-to-log?
;          (swap! history #(subvec
;                            ; don't add duplicate expressions to the history
;                            (if (= expr (last %))
;                              %
;                              (conj % expr))
;                            (-> % count (- history/max-history) (max 0))))
          ;(retain-expr-fn expr))
        expr))))
      
      
