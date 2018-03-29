(defproject otm-graphical-editor "0.1.0"
  :description "EF Exercise"
  :license {:name "GNU General Public License (GPL) version 3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [com.cemerick/url "0.1.1"]]
  :main ^:skip-aot ef.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
