(ns io.aviso.frappe.fs
  "Utilities for frappe for access to resource files."
  (:require [io.aviso.frappe :as f :refer [cell]]
            [clojure.java.io :as io])
  (:import [java.io File]))

(defn path->file-cell
  [file-cache path file]
  (let [file-cell (or (get @file-cache path)
                      (let [new-cell (cell nil)]
                        (swap! file-cache assoc path new-cell)
                        new-cell))]
    (f/force! file-cell {:path        path
                         :file        file
                         :modified-at (and file (.lastModified file))})))

(defn- relative-path
  [^File root ^File file]
  (let [root-path (.getCanonicalPath root)
        file-path (.getCanonicalPath file)]
    (subs file-path (inc (.length root-path)))))

(defn find-file-cells
  "Finds files within a root directory, starting from a root path. Files must pass though
  the file filter.

  file-filter is a function that accepts a File instance and returns a truthy; only truthy
  files will be included in the result (and directories, never).
  The default file-filter includes everything.

  Returns a map of path (relative to the root-path) to file map cell."
  ([root-path file-cache]
   (find-file-cells root-path file-cache (constantly true)))
  ([root-path file-cache file-filter]
   (let [root-file (io/as-file root-path)
         xf        (comp (remove #(.isDirectory %))
                         (filter file-filter)
                         (map #(let [path (relative-path root-file %)]
                                [path (path->file-cell file-cache path %)])))]
     (->>
       root-file
       file-seq
       (into {} xf)))))




