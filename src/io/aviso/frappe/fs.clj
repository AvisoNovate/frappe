(ns io.aviso.frappe.fs
  "Utilities for frappe for access to resource files."
  (:require [io.aviso.frappe :as f :refer [cell]]
            [clojure.java.io :as io])
  (:import [java.io File]))

(defn file-info
  "Given a path (relative to a root folder) and a File, returns a map of data about that file."
  [^String path ^File file]
  (let [[folder-path file-name] (let [slashx (.lastIndexOf path "/")]
                                  (if (neg? slashx)
                                    ["" path]
                                    [(subs path 0 slashx)
                                     (subs path (inc slashx))]))
        dotx      (.lastIndexOf file-name ".")
        base-name (subs file-name 0 dotx)
        extension (subs file-name (inc dotx))]
    {:path        path
     :file        file
     :folder-path folder-path                               ; path up to, not including, last slash
     :file-name   file-name                                 ; path from past last slash
     :base-name   base-name                                 ; file-name w/o extension (e.g. "index")
     :extension   extension                                 ; extension w/o dot (e.g., "html")
     :modified-at (and file (.lastModified file))}))

(defn- relative-path
  [^File root ^File file]
  (let [root-path (.getCanonicalPath root)
        file-path (.getCanonicalPath file)]
    (subs file-path (inc (.length root-path)))))

(defn with-extension
  "Returns a File filter that matches by extension."
  [ext]
  (let [suffix (str "." ext)]
    (fn [^File file]
      (-> file
          .getName
          (.endsWith suffix)))))

(defn find-files
  "Finds files within a root directory, starting from a root path. Files must pass though
  the file filter.

  file-filter is a function that accepts a File instance and returns a truthy; only truthy
  files will be included in the result (and directories, never).
  The default file-filter includes everything.

  Returns a map of path (relative to the root-path) to file map cell."
  ([root-path]
   (find-files root-path (constantly true)))
  ([root-path file-filter]
   (let [root-file (io/as-file root-path)
         xf        (comp (remove #(.isDirectory %))
                         (filter file-filter)
                         (map #(let [path (relative-path root-file %)]
                                [path (file-info path %)])))]
     (->>
       root-file
       file-seq
       (into {} xf)))))




