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
                        new-cell))
        [folder-path file-name] (let [slashx (.lastIndexOf path "/")]
                                  (if (neg? slashx)
                                    ["" path]
                                    [(subs path 0 slashx)
                                     (subs path (inc slashx))]))
        dotx      (.lastIndexOf file-name ".")
        base-name (subs file-name 0 dotx)
        extension (subs file-name (inc dotx))]
    (f/force! file-cell {:path        path
                         :file        file
                         :folder-path folder-path
                         :file-name   file-name
                         :base-name   base-name
                         :extension   extension
                         :modified-at (and file (.lastModified file))})))

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

(defn find-file-cells
  "Finds files within a root directory, starting from a root path. Files must pass though
  the file filter.

  file-filter is a function that accepts a File instance and returns a truthy; only truthy
  files will be included in the result (and directories, never).
  The default file-filter includes everything.

  Returns a cell containing a map of path (relative to the root-path) to file map cell."
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
       (into {} xf)
       cell))))




