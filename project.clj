(defproject hackakl_where_is_my_bus "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2173"]
                [datascript "0.1.3"]
                [reagent "0.4.2"]]

  :plugins [[lein-cljsbuild "1.0.2"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "hackakl_where_is_my_bus"
              :source-paths ["src"]
              :compiler {
                :output-to "hackakl_where_is_my_bus.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
