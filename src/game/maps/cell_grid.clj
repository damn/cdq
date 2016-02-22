(ns game.maps.cell-grid
  (:require [data.grid2d :as grid])
  (:use utils.core
        game.session
        game.components.core
        game.maps.data))

(defn get-cell-grid [] (:cell-grid (get-current-map-data)))
(defn get-jcell-grid [] (force (:j-cell-grid (get-current-map-data))))

(defn create-cell [position blocks]
  {:pre [(#{#{}
            #{:ground}
            #{:ground :air}} blocks)]}
  (atom
    {:posi position
     :middle (translate-to-tile-middle position)
     :blocks blocks
     :body-ids #{}
     :occupied #{}}))

(defn occupied-by-other?
  "returns true if there is some solid body with center-tile = this cell
   or a multiple-cell-size body which touches this cell."
  [cell body]
  (seq (filter is-solid? (disj (:occupied @cell) body))))

(defn cell-blocked? [cell movement-type] ; TODO cell-blocks?
  (or
    (not cell)
    (contains? (:blocks @cell) movement-type)))

(defn get-cell
  "returns nil when not existing cell (outside map bounds or undefined area (behind walls))"
  [posi]
  (get (get-cell-grid) (int-posi posi)))

(defn get-cells
  "see (doc get-cell). Use int-posis for this! Faster than (map get-cell posis)"
  [posis]
  (map (get-cell-grid) posis))

;TODO just use a delay/force?
(defn cached-get-adjacent-cells [cell]
  (if-let [result (:adjacent-cells @cell)]
    result
    (let [result (-> @cell :posi grid/get-8-neighbour-positions get-cells)]
      (swap! cell assoc :adjacent-cells result)
      result)))

(def- jcell-grid-movement-type :air)

(defn create-jcell-grid [grid]
  (let [j-cell-grid (make-array Boolean/TYPE (grid/width grid) (grid/height grid))]
    (doseq [[[x y] cell] grid]
      (aset j-cell-grid x y
            (boolean (cell-blocked? cell jcell-grid-movement-type))))
    j-cell-grid))

(defn get-map-w [] (grid/width (get-cell-grid)))
(defn get-map-h [] (grid/height (get-cell-grid)))

(defn inside-map? [tile] (get-cell tile))

(def ^:private listeners (atom []))
(def listeners-session (atom-session listeners :save-session false))

(defn add-cell-blocks-changed-listener [f] (swap! listeners conj f))

(defn cell-blocks-changed-update-listeners [] (runmap #(%) @listeners)) ; TODO runmap eval ? memfn invoke

(defn change-cell-blocks
  "do not change to blocking while game running or bodies may be walled in.
   after changing update the listeners!!" ; vlt cell-seq �bergeben und danach wird hier update-listeners gecallt.
  [cell new-blocks]
  (swap! cell assoc-in [:blocks] new-blocks)
  (let [[x y] (:posi @cell)]
    (aset (get-jcell-grid) x y (boolean (cell-blocked? cell jcell-grid-movement-type))))) ; TODO same code as in create-jcell-grid

(defn get-body-ids [cell]
  (if cell (:body-ids @cell) #{}))

(defn- get-body-ids-from-cells [cells]
  (distinct (mapcat get-body-ids cells)))

(defn get-bodies-from-cells [cells]
  (map get-entity (get-body-ids-from-cells cells)))

;;

(defn- in-cell? [cell body]
  (get (get-body-ids cell) (get-id body)))

(defn add-body [cell body]
  {:pre [(not (in-cell? cell body))]}
  (swap! cell update-in [:body-ids] conj (get-id body)))

(defn remove-body [cell body]
  {:pre [(in-cell? cell body)]}
  (swap! cell update-in [:body-ids] disj (get-id body)))

;;

; TODO use mapgen.nad code?
(defn is-diagonal? [from to]
  (let [[fx fy] (:posi @from)
        [tx ty] (:posi @to)
        xdir (- tx fx)
        ydir (- ty fy)]
    (diagonal-direction? [xdir ydir])))

(def ^:private allowed-gridkeys #{:undefined :ground :wall :airwalkable})

(defn create-grid-from-gen-grid
  "blockprops-map is a map of {:undefined :wall :ground} to the blocks like #{:wall :ground},
  can also be nil - then no cell is created."
  [grid blockprops-map calc-sprite-idx details-calc-sprite-idx]
  {:pre [(= allowed-gridkeys (set (keys blockprops-map)))]}
  (let [ks (set (grid/cells grid))]
    (assert (every? allowed-gridkeys ks)
            (str "Allowed ks: " allowed-gridkeys "\n and keys in grid: " ks)))
  (grid/transform
    grid
    (fn [posi value]
      (when-let [blocks (get blockprops-map value)]
        (let [cell (create-cell posi blocks)]
          (swap! cell assoc
                 :sprite-posi (calc-sprite-idx grid posi value)
                 :details-sprite-posi (when details-calc-sprite-idx
                                        (details-calc-sprite-idx grid posi value)))
          cell)))))
