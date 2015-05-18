(defproject io.aviso/frappe "0.1.0"
            :description "Functional/Reative Programming"
            :dependencies [[org.clojure/clojure "1.7.0-beta3"]
                           [io.aviso/toolchest "0.1.2"]
                           [medley "0.5.3"]
                           [com.stuartsierra/dependency "0.1.1"]]
            :profiles {:dev {
                             :dependencies [[io.aviso/pretty "0.1.18"]]
                             }}
            :codox {:src-dir-uri               "https://github.com/AvisoNovate/frappe/blob/master/"
                    :src-linenum-anchor-prefix "L"
                    :defaults                  {:doc/format :markdown}})
