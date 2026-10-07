(ns utils.counter)

(defn create-counter
  ([]        (atom {:current 0}))
  ([maxtime] (atom {:current 0,:max maxtime})))

(defn reset-counter! [counter]
  (swap! counter assoc-in [:current] 0))

(defn update-counter
  "updates counter. if maxtime reached, resets current to 0 and returns the last current value,
   else returns nil."
  [counter delta]
  (swap! counter update-in [:current] + delta)
  (let [{current :current maxtime :max} @counter]
    (when (and maxtime (>= current maxtime))
      (let [last-current current]
        (reset-counter! counter)
        last-current))))
