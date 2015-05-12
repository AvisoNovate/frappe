(ns io.aviso.frappe
  "A take on server-side functional/reactive programming, with an eye towards smart publishing."
  (:require [io.aviso.toolchest.macros :refer [cond-let]])
  (:import [clojure.lang IDeref]))

(def ^:dynamic *defining-cell*
  "Identifies the cell that is currently being defined (and executed) to establish dependencies."
  nil)

(def ^:private empty-reaction
  {:dirty-cells       #{}
   :pending-callbacks []})

(def ^:dynamic *reaction*
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

  (add-dependency! [this other]
    "Adds the other cell as a dependency of this cell. This allows the order in which cells are
    recalculated after a change to be optimized.

    Returns this cell.")

  (dependencies [this]
    "Returns the set of dependencies for this cell (used to order recalculations).")

  (add-dependant! [this other]
    "Adds another cell as a dependant of the this cell. When this cell's value changes,
    each dependeant is notified to recompute its value.

    Returns this cell."))

(defn- add-dirty-cell!
  [cell]
  ;; This is a bit naive and we need something a lot smarter for large graphs.  If node X has a dependency on Y and Q,
  ;; and Y has a dependency on Q, then a change to Q will cause X and Y to be recalculated ... and then X will be
  ;; recalculated a second time. So we need to leverage the graph a bit more, to ensure that X is recalculated only
  ;; after Y and Q are recalculated.
  (swap! *reaction* update :dirty-cells conj cell))

(defn- add-callback!
  [callback cell-value]
  (swap! *reaction* update :pending-callbacks conj [callback cell-value]))

(defn- finish-reaction!
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
        (reset! *reaction* empty-reaction)
        (doseq [cell dirty-cells]
          (recalc! cell))
        (doseq [[callback value] pending-callbacks]
          (callback value))

        ;; Those cells & callbacks may have changed other things resulting in
        ;; further dirty cells & callbacks.
        (recur (inc i))))))

(defn cell*
  "Creates a new cell around a function of no arguments that computes its value.
  The function is almost always a closure that can reference other cells.

  A cell may be dereferenced (use the @ reader macro, or deref special form)."
  [f]
  (let [dependants       (atom #{})                         ; recalced when this cell changes
        dependencies     (atom #{})                         ; reverse of dependenant-cells
        change-listeners (atom [])
        current-value    (atom nil)
        cell             (reify

                           Cell

                           (on-change! [this callback]
                             (swap! change-listeners conj callback)
                             (callback @current-value)
                             this)

                           (add-dependant! [this other]
                             (swap! dependants conj other)
                             this)

                           (add-dependency! [this other]
                             (swap! dependencies conj other)
                             this)

                           (dependencies [_] @dependencies)

                           (recalc! [this]
                             (force! this (f)))

                           (force! [this new-value]
                             (cond
                               ;; In many cases, the new computed value is the same as the prior value so
                               ;; there's no need to go further.
                               (= @current-value new-value)
                               nil

                               ;; Create, as needed, a reactive transaction.
                               (nil? *reaction*)
                               (binding [*reaction* (atom empty-reaction)]
                                 (force! this new-value)
                                 (finish-reaction!))

                               :else
                               (do
                                 (reset! current-value new-value)
                                 ;; These behaviors are deferred to help avoid redundant work.
                                 (doseq [listener @change-listeners]
                                   (add-callback! listener new-value))
                                 (doseq [cell @dependants]
                                   (add-dirty-cell! cell))))

                             this)

                           IDeref

                           (deref [this]
                             (when *defining-cell*
                               ;; This cell was dereferenced while defining another cell. That means
                               ;; the cell being defined is a dependant of this cell, and should recalc
                               ;; when this cell changes.
                               (add-dependant! this *defining-cell*)
                               (add-dependency! *defining-cell* this))

                             @current-value)

                           Object
                           (toString [_]
                             (str "io.aviso.frappe.Cell[" @current-value "]")))]

    (binding [*defining-cell* cell]
      ;; Exercise the function to collect dependencies.
      ;; In some cases, the function may need to explicitly deref cells that may otherwise
      ;; only be invoked in some cases, or lazily.
      (reset! current-value (f)))
    cell))

(defmacro cell
  [& body]
  "Creates a cell, wrapping the body up as the necessary function used by [[cell*]]."
  `(cell* (fn [] ~@body)))

(comment
  (do
    (require '[clojure.string :as str])
    (def s (cell "Howard"))
    (def u (cell
             (println "recalc: u")
             (str/upper-case @s)))
    (def r (cell
             (println "recalc: r")
             (str (apply str (reverse @u))
                  "-"
                  @s)))
    (on-change! u #(println "u changed to" %))
    (on-change! r #(println "r changed to" %)))
  (force! s "Suzy")
  (force! s "Jacob")
  (force! s "JACob")
  )