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

(defn- path->file
  [path]
  (-> path io/resource io/file))

(defn find-resources
  "Finds resources on the classpath starting from a root path. Files must pass though
  the file filter.

  Returns a map of path (relative to the root-path) to file map cell."
  [root-path file-cache file-filter]
  (let [root-file (path->file root-path)
        xf        (comp (remove #(= root-file %))
                        (filter file-filter)
                        (map #(let [path (relative-path root-file %)]
                               [path (path->file-cell file-cache path %)])))]
    (->>
      root-file
      file-seq
      (into {} xf))))




