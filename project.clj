(defproject io.aviso/frappe "0.1.0"
            :description "Functional/Reative Programming"
            :dependencies [[org.clojure/clojure "1.7.0-beta2"]
                           [io.aviso/toolchest "0.1.2"]
                           [com.stuartsierra/dependency "0.1.1"]]
            :profiles {:dev {
                             :dependencies [[io.aviso/pretty "0.1.18"]]
                             }})
