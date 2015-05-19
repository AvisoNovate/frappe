(ns io.aviso.frappe
  "A take on server-side functional/reactive programming, with an eye towards smart publishing."
  (:require [io.aviso.toolchest.macros :refer [cond-let]]
            [com.stuartsierra.dependency :as dep]
            [clojure.pprint :as pp]
            [medley.core :as medley]
            [clojure.set :as set])
  (:import [clojure.lang IDeref]
           [java.io Writer]
           [java.util.concurrent.atomic AtomicInteger]))

(def ^:dynamic ^:no-doc *defining-cell*
  "Identifies the cell that is currently being defined (and executed) to establish dependencies."
  nil)

(def ^:private empty-reaction
  {:dirty-cells       #{}
   :pending-recalcs   #{}
   :pending-callbacks []})

(def ^:dynamic ^:no-doc *reaction*
  "A atom that tracks information about the current reactive transaction."
  nil)

(defprotocol Cell
  "Defines the behavior of a Cell, which is a value computed from other Cells. These are largely internal
  and should not be directly invoked by user code."

  (on-change! [this callback]
    "Adds a callback to the cell; the callback is only invoked when the value for the cell changes.
    The callback is a function of a single argument: the new value for the cell.

    The callback is also invoked when it is first added to the Cell.

    The order in which callbacks are invoked (when changes do occur) is not specified.

    Returns the cell.")

  (recalc! [this]
    "Forces the cell to recalculate its value, when a dependent cell changes. This will
    in turn invoke [[force!]], which can then inform other cells and listeners.")

  (force! [this new-value]
    "Forces a change to a cell, which should only occur for edges (cells with constant values
    and no dependencies).

    The current value of the cell will change, and listeners and dependent cells will be notified.

    Returns the cell.")

  (add-dependant! [this other]
    "Adds another cell as a dependant of the this cell. When this cell's value changes,
    each dependeant is notified to recompute its value.

    Returns this cell.")

  (dependants [this]
    "Returns the dependants of this cell; cells whose value depends on this cell."))

(defn- add-if-not-pending-recalc [reaction cell]
  (if (-> reaction :pending-recalcs (contains? cell))
    reaction
    (update reaction :dirty-cells conj cell)))

(defn- add-dirty-cell!
  [cell]
  (swap! *reaction* add-if-not-pending-recalc cell))

(defn- add-callback!
  [callback cell-value]
  (swap! *reaction* update :pending-callbacks conj [callback cell-value]))

(defn ^:no-doc finish-reaction!
  []
  (loop [i 0]
    (cond-let

      (= 10 i)
      (throw (ex-info "Reaction did not settle after 10 cycles."
                      {:reaction @*reaction*}))

      [{:keys [dirty-cells pending-callbacks]} @*reaction*]

      (and (empty? dirty-cells)
           (empty? pending-callbacks))
      nil

      :else
      (do
        ;; This is an optimization designed to minimize unnecessary (or transitional) recalculations of a cell.
        ;; If cell A depends on cell B and cell C, then order counts.  You want cells B and C to recalc before A.
        ;; We use dependency ordering here; we identify that A is a dependant of B (that is, A depends on B)
        ;; which ensures that A will recalc after B. Same for C. We cull out edges
        (let [dependency-pairs (for [cell      dirty-cells
                                     dependant (dependants cell)
                                     :when (dirty-cells dependant)]
                                 [cell dependant])
              graph            (reduce (fn [g [dependency cell]] (dep/depend g cell dependency))
                                       (dep/graph)
                                       dependency-pairs)
              comparator       (dep/topo-comparator graph)
              ;; Order dependants before dependencies, with everything else in arbitrary order at
              ;; the end.
              recalc-order     (sort comparator dirty-cells)]

          ;; Any cell in the current iteration that gets marked dirty will be ignored and not
          ;; added to the next iteration.
          (reset! *reaction* (assoc empty-reaction :pending-recalcs dirty-cells))

          (doseq [cell recalc-order]
            (recalc! cell)))

        (doseq [[callback value] pending-callbacks]
          (callback value))

        ;; Those cells & callbacks may have changed other things resulting in
        ;; further dirty cells & callbacks.
        (recur (inc i))))))

(defmacro reaction
  "A reaction is a reactive transaction; it is useful when invoking [[force!]] on several
  cells to handle the notifications as a single unit, which reduces the number of iterations necessary
  to propogate changes through the graph, and helps reduce the chances of an redundant recalc."
  [& body]
  `(binding [*reaction* (atom empty-reaction)]
     (let [result# (do ~@body)]
       (finish-reaction!)
       result#)))

(defrecord CellData [dependants change-listeners current-value])

(defrecord ^:no-doc CellImpl [id f cell-data]

  Cell

  (on-change! [this callback]
    (swap! cell-data update :change-listeners conj callback)
    (callback @this)
    this)

  (add-dependant! [this other]
    (swap! cell-data update :dependants conj other)
    this)

  (dependants [_] (:dependants @cell-data))

  (recalc! [this]
    (force! this (f)))

  (force! [this new-value]
    (cond
      ;; In many cases, the new computed value is the same as the prior value so
      ;; there's no need to go further.
      (= (:current-value @cell-data) new-value)
      nil

      ;; Create, as needed, a reactive transaction.
      (nil? *reaction*)
      (reaction
        (force! this new-value))

      :else
      (let [{:keys [change-listeners dependants]} (swap! cell-data assoc :current-value new-value)]
        ;; These behaviors are deferred to help avoid redundant work.
        (doseq [listener change-listeners]
          (add-callback! listener new-value))
        (doseq [cell dependants]
          (add-dirty-cell! cell))))

    this)


  IDeref

  (deref [this]
    (when *defining-cell*
      ;; This cell was dereferenced while defining another cell. That means
      ;; the cell being defined is a dependant of this cell, and should recalc
      ;; when this cell changes.
      (add-dependant! this *defining-cell*))

    (:current-value @cell-data)))

(defmethod print-method CellImpl [cell ^Writer w]
  (.write w (str "io.aviso.frappe.Cell[" (:id cell) "]")))

(defmethod pp/simple-dispatch CellImpl
  [cell]
  (let [cell-data (-> cell :cell-data deref)]
    (pp/simple-dispatch (assoc cell-data :id (:id cell)))))

(defonce ^:private next-cell-id (AtomicInteger. 0))

(defn cell*
  "Creates a new cell around a function of no arguments that computes its value.
  The function is almost always a closure that can reference other cells.

  A cell may be dereferenced (use the @ reader macro, or deref special form)."
  [f]
  (let [cell (->CellImpl (.incrementAndGet next-cell-id)
                         f
                         (atom (->CellData #{} [] nil)))]
    ;; This forces an evaluation of the cell, which will trigger any dependencies, letting
    ;; us build up the graph.
    (binding [*defining-cell* cell]
      (force! cell (f)))

    cell))

(defmacro cell
  "Creates a cell, wrapping the body up as the necessary function used by [[cell*]]."
  [& body]
  `(cell* (fn [] ~@body)))

;;; This is feeling like something that should be dealt with using transducers.

(defn map-cells
  "Given a cell that contains a map of keys to cells, returns a new map with the same keys,
  but new cells with the cell values transformed by the value-transform function."
  [source-cell value-transform]
  (cell (medley/map-vals
          (fn [value-cell]
            (cell (value-transform @value-cell)))
          @source-cell)))

(defn project-cell
  "Given a map of keys to cells, returns a new cell with the same value as the cell
  with the given key."
  [source-cell k]
  (cell @(get @source-cell k)))

(defn delta-map-cell
  "Given a source cell containing a map of keys to cells, returns a new cell
  containing a map with two keys:  :added, :removed.

  :added is map of new keys to value cells, for keys that did not exist previously.

  :removed is map of prior keys to value cells, for keys that were removed."
  [source-cell]
  (let [prior-map (atom nil)]
    (cell
      (let [source-map      @source-cell
            source-map-keys (set (keys source-map))
            prev-map        @prior-map
            prev-map-keys   (set (keys prev-map))

            added-keys      (set/difference source-map-keys prev-map-keys)
            removed-keys    (set/difference prev-map-keys source-map-keys)]
        ;; Save the source-map for the next iteration:
        (reset! prior-map source-map)

        {:added   (select-keys source-map added-keys)
         :removed (select-keys prev-map removed-keys)}))))

(defn select-cells
  [key-filter source-cell]
  (cell
    (medley/filter-keys key-filter @source-cell)))

(comment
  (do
    (require '[clojure.string :as str])
    (use 'clojure.pprint)
    (use 'clojure.repl)
    (use 'criterium.core)
    (def s (cell "Howard"))
    (def u (cell
             (println "recalc: u")
             (str/upper-case @s)))
    (def r (cell
             (println "recalc: r")
             (apply str (reverse @u))))
    (def c (cell
             (println "recalc: c")
             (str @r
                  "-"
                  @s)))
    (on-change! u #(println "u changed to" %))
    (on-change! c #(println "c changed to" %))
    (on-change! r #(println "r changed to" %)))
  (force! s "Suzy")
  (force! s "Jacob")
  (force! s "JACob")
  )