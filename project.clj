(require 'cemerick.pomegranate.aether)
(cemerick.pomegranate.aether/register-wagon-factory!
  "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))
(defproject CSNePS "1.0.0-SNAPSHOT"
  :description "CSNePS Knowledge Representation and Reasoning System"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/core.memoize "0.5.9"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.clojure/tools.trace "0.7.9"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 [net.sf.jung/jung-graph-impl "2.1.1"]
                 [net.sf.jung/jung-api "2.1.1"]
                 [net.sf.jung/jung-visualization "2.1.1"]
                 [net.sf.jung/jung-io "2.1.1"]
                 [net.sf.jung/jung-algorithms "2.1.1"]
                ;;  [javax.media/jai_core "1.1.3"]
                 ;; https://mvnrepository.com/artifact/javax.media/jai_core
                 ;[net.sf.jung/jung-jai "2.0.1"]
                 ;[net.sf.jung/jung-3d "2.0.1"]
                 [junit/junit "3.8.2"]
                 [jdom/jdom "1.0"]
                 [org.freehep/freehep-graphics2d "2.4"]
                 [org.freehep/freehep-graphicsio "2.4"]
                 [org.freehep/freehep-graphicsbase "2.4"]
                 [org.freehep/freehep-io "2.2.2"]
                 [org.freehep/freehep-graphicsio-emf "2.4"]
                 [org.freehep/freehep-graphicsio-java "2.4"]
                 [org.freehep/freehep-graphicsio-pdf "2.4"]
                 [org.freehep/freehep-graphicsio-ps "2.4"]
                 [org.freehep/freehep-graphicsio-svg "2.4"]
                 [org.freehep/freehep-graphicsio-swf "2.4"]
                 [org.jpedal/jpedal-lgpl "4.74b27"]
                 [org.swinglabs/swingx "1.6.1"]
                 [net.xeon/jspf-core "1.0.2"]
                 [org.clojure/tools.cli "0.4.1"]
                 [reply/reply "0.4.4"]
                 [aleph "0.4.6"]
                 [gloss "0.2.6"]]
  :dev [[org.clojure/tools.namespace "0.2.4"]]
  :repositories {"FreeHEP" "http://java.freehep.org/maven2"
                 "mvnrepo" "https://mvnrepository.com"
                 "local" ~(str (.toURI (java.io.File. "local_maven_repo")))}
  :plugins [[lein-swank "1.4.5"]]
  :source-paths ["src/clj/"]
  :source-path "src/clj/"
  :java-source-paths ["src/jvm/"] ;leiningen 2 compat.
  :java-source-path "src/jvm/" ;leiningen 1.x compat.
  ;:project-init (require 'clojure.pprint) 
  :repl-options [:print clojure.core/println] ;[:print clojure.pprint/pprint]
  ;:jvm-opts ["-server"]
  :main csneps.core.snuser
  :profiles {:uberjar {:aot :all}})
