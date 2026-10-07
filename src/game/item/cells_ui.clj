(ns game.item.cells-ui
  (:require
    [engine.render :as color :refer [draw-grid draw-image fill-rect render-readable-text rgbcolor set-color]]
    [game.components.render :refer [rendering]]
    [game.components.ingame-loop :refer [ingame-loop-comp]]
    [engine.input :refer [get-mouse-pos]]
    [game.ingame-gui :refer [background-color foreground-color]]
    [data.grid2d :refer [height width]]
    [game.item.colors :refer [equip-boni-item-color]]
    [game.item.in-hand :refer [is-item-in-hand? item-in-hand]]
    [game.item.grids :refer [background-icons cell-h cell-w hks-cells item-grids]]
    [game.item.cells :refer [cell-empty-and-allows-item? cell-filled-and-allows-item? inc-count-of-item? item-in-use?]]))

(defn- get-item-textseq [cell]
  (let [item @(:item cell)
        itemname (or (:pretty-name item) (:name item))
        color (or (:color item) color/lightGray)]
    (concat
      [color
       (str itemname
            (when-let [cnt (:count item)]
              (str " (" cnt ")")))
       color/white
       (:info item)
       equip-boni-item-color]
      (when-let [boni (:equip-boni item)]
        (map :info boni)))))

(defn- get-cell-renderposi
  ([cell]
    (get-cell-renderposi (:posi cell) (:grid-type cell)))
  ([[cx cy] grid-type]
    (let [grid (grid-type item-grids) ; :keys
          gx (:rx grid)
          gy (:ry grid)]
      (get-cell-renderposi gx gy cx cy)))
  ([gx gy cx cy]
    [(+ gx (* cx cell-w))
     (+ gy (* cy cell-h))]))

(defn get-mouseover-item-cell ; TODO ??
  ([]
    (some
      #(when ((:visible-check %))
         (get-mouseover-item-cell (:grid-type %)))
      (vals item-grids)))
  ([renderx rendery item-grid]
    (let [[mx my] (get-mouse-pos)
          cx (/ (- mx renderx) cell-w)
          cy (/ (- my rendery) cell-h)
          cellposi [(int cx) (int cy)]]
      (if-not (or (< cx 0) (< cy 0)) ; da (int -0.X) wird zu 0
        (get item-grid cellposi))))
  ([grid-type]
    (let [item-grid (grid-type item-grids)
          gx (:rx item-grid)
          gy (:ry item-grid)]
      (get-mouseover-item-cell gx gy item-grid))))

(defn mouse-over-an-item-cell? []
  (get-mouseover-item-cell))

(defn- item-droppable-in-cell? [item cell]
  (or
    (cell-empty-and-allows-item? cell item)
    (inc-count-of-item? @(:item cell) item)
    (cell-filled-and-allows-item? cell item)))

(defn- render-item-tooltip [g]
  (when-let [cell (get-mouseover-item-cell)]
    (when @(:item cell)
      (let [[rx ry] (get-cell-renderposi cell)
            textseq (get-item-textseq cell)]
        (if (= :belt (:grid-type cell))
          (apply render-readable-text g rx ry :above true textseq)
          (apply render-readable-text g rx (+ ry cell-h) textseq))))))

(ingame-loop-comp :item-tooltip
  (rendering :tooltips [g c]
    (render-item-tooltip g)))

(def ^:private item-cells-bg-color (.darker background-color 0.5))
(def ^:private item-cells-fg-color foreground-color)

(def ^:private droppable-color (rgbcolor :g 0.6 :a 0.8))
(def ^:private not-allowed-color (rgbcolor :r 0.6 :a 0.8))

(defn- render-cell-droppable-indicator [g cell rx ry]
  (fill-rect g rx ry cell-w cell-h
    (if (item-droppable-in-cell? @item-in-hand cell)
      droppable-color
      not-allowed-color)))

(defn- render-item-in-cell [g item cell rx ry]
  (when (item-in-use? cell)
    (fill-rect g rx ry cell-w cell-h color/red))
  (draw-image (:image item) rx ry)
  (when-let [cnt (:count item)]
    (render-readable-text g rx ry cnt)))

(defn- render-item-grid
  "renders background, a grid and item-images/count if items in cell."
  [g grid-type]
  (let [grid (grid-type item-grids) ; :keys
        gridw (width grid)
        gridh (height grid)
        x (:rx grid)
        y (:ry grid)
        w (* gridw cell-w)
        h (* gridh cell-h)
        mouseover-item-cell (get-mouseover-item-cell)]
    (fill-rect g x y w h item-cells-bg-color)
    (doseq [[[cx cy] cell] grid]
      (let [[rx ry] (get-cell-renderposi x y cx cy)
            item @(:item cell)]
        (when (and (is-item-in-hand?) (= cell mouseover-item-cell))
          (render-cell-droppable-indicator g cell rx ry))
        (when item
          (render-item-in-cell g item cell (inc rx) (inc ry))) ; cell size > item-size so the item is not behind the grid lines
        (when (and (not item) (get background-icons grid-type))
          (draw-image (get background-icons grid-type) (inc rx) (inc ry)))))
    (set-color g item-cells-fg-color)
    (draw-grid g x y gridw gridh cell-w cell-h)))

(defn- render-belt-hotkeys [g]
  (doseq [hotkey (keys hks-cells)
          :let [cell-posi (get hks-cells hotkey)
                [rx ry] (get-cell-renderposi cell-posi :belt)]]
    (render-readable-text g (+ rx (/ cell-w 2)) ry :above true :centerx true (name hotkey))))

(ingame-loop-comp :item-cells
  (rendering [g c]
    (dorun (map
      #(when ((:visible-check %))
         (render-item-grid g (:grid-type %))) ; give grid as argument ...
      (vals item-grids)))
    (render-belt-hotkeys g)))
