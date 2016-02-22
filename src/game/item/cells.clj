(ns game.item.cells ; more than cells
  (:require [engine.render :as color])
  (:use
    utils.core
    (engine core input render)
    (game settings mouse-cursor ingame-gui)
    data.grid2d
    (game.components core
      [render :only (rendering)]
      [ingame-loop :only (ingame-loop-comp)])
    game.components.skills.core))

; diablo 2 gold:  144 136 88
; blue:           72 80 184
(defcolor equip-boni-item-color :r 0.28 :g 0.31 :b 0.72 :brighter 0.5)
(defcolor gold-item-color :r 0.56 :g 0.53 :b 0.34 :brighter 0.5)

;;

(def item-in-hand (atom nil))

(defn is-item-in-hand? [] @item-in-hand)

(defn set-item-in-hand [item]
  (reset! item-in-hand item)
  (set-mouse-cursor (get-scaled-copy (:image item) screen-scale) (* 8 screen-scale) (* 8 screen-scale)))

(defn empty-item-in-hand []
  (reset! item-in-hand nil)
  (reset-default-mouse-cursor))

;;

(defn get-item [cell] @(:item cell))

(declare item-grids) ; used either with :keyword or vals -> encapsulate vals somehow?

(defn get-cells-from [grid-type] ; really strange name ...
  (cells (grid-type item-grids)))

(defn get-equiped-hands-item []
  (get-item
    (get (:hands item-grids) [0 0]))) ; (-> item-grids :hands [0 0])

(defn empty-item-cell? [cell]
  (nil? (get-item cell)))

(defn cell-allows-item? [{allows :allows-type :as cell} item]
  (or
    (= :all allows)
    (= (:type item) allows)))

(defn cell-empty-and-allows-item? [cell item]
  (and
    (empty-item-cell? cell)
    (cell-allows-item? cell item)))
	
(defn cell-filled-and-allows-item? [cell item]
  (and
    (get-item cell)
    (cell-allows-item? cell item)))

(defn inc-count-of-item? [item1 item2]
  (and
    item1
    (:count item1)
    (:count item2)
    (= (:name item1) (:name item2))))

(defn- check-apply-boni [item cell k]
  (let [components player-body]
    (when (:is-equipment cell)
      (swap! components thread-through (map k (:equip-boni item)))
      (when-let [equip (k item)]
        (swap! components equip)))))

(defn add-item-to-cell [item cell]
  {:pre [(cell-empty-and-allows-item? cell item)]}
  (reset! (:item cell) item)
  (check-apply-boni item cell :equip))

(defn remove-item-from-cell [cell]
  (let [item (get-item cell)] ; get the item bevor setting it nil so that :unequip works!
    (reset! (:item cell) nil)
    (check-apply-boni item cell :unequip)))

(defn empty-all-item-grids []
  (doseq [grid (vals item-grids)
          cell (cells grid)]
      (remove-item-from-cell cell)))

(defn remove-one-item [cell]
  (let [item (get-item cell)]
    (if-let [cnt (:count item)]
      (if (> cnt 1)
        (swap! (:item cell) update-in [:count] dec)
        (remove-item-from-cell cell))
      (remove-item-from-cell cell))))

(defn remove-one-item-from [grid-type item-name]
  {:pre [(some #{item-name}
           (map #(:name (get-item %))
             (get-cells-from :belt)))]}
  (remove-one-item
    (find-first
      #(= item-name (:name (get-item %)))
      (get-cells-from :belt))))

(defn- get-item-textseq [cell]
  (let [item (get-item cell)
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

(defn item-not-in-use? [cell]
  (let [item (get-item cell)
        skillmanager (get-component player-body :skillmanager)
        active-skill (get-active-skill skillmanager)
        in-use (and
                 (:is-equipment cell) item (is-attacking? skillmanager)
                 (or
                   (and (:skill item) (= active-skill (:skill item)))
                   (and (:melee-weapon item) (= (get-equiped-hands-item) item) (:is-melee active-skill))))]
    (not in-use)))

(defn- item-in-use? [cell]
  (not (item-not-in-use? cell)))

(defn- create-empty-item-cell [posi allows-type grid equipment]
  {:item (atom nil)
   :is-equipment equipment
   :allows-type allows-type
   :posi posi
   :grid-type grid})

; 'inherits' from VectorGrid and just valAt changed...
; other ideas: item-grid data as metadata of VectorGrid
; protocol ItemGrid extends VectorGrid (get-rx (get-grid-type etc?
; separate [item-grid item-grid-data]
(deftype ItemGrid [grid2d m]
  data.grid2d.Grid2D
  (cells [this] (cells grid2d))
  (width [this] (width grid2d))
  (height [this] (height grid2d))

  clojure.lang.Seqable
  (seq [this] (seq grid2d))

  clojure.lang.ILookup
  (valAt [this k]
    (if (keyword? k)
      (k m)
      (.valAt grid2d k))))

(def item-grids {})

(defnks add-item-grid [:w :h :rx :ry :allows-type :grid-type :is-equipment-cell :visible-check]
  {:pre [(not-any? #{grid-type} (keys item-grids))]}
  (alter-var-root #'item-grids assoc grid-type
                  (ItemGrid. (create-grid w h #(create-empty-item-cell % allows-type grid-type is-equipment-cell))
                             (select-keys argsmap [:visible-check :grid-type :rx :ry :allows-type]))))

(def- inventory-cells-x 6)
(def- inventory-cells-y 4)

(def- cell-w 17) ; cells = item-size+1 so items fit inside grid lines, else top and left pixel line of items not visible
(def- cell-h 17)

(def- borderpx 2)
(def- inventory-width (+ (* 2 borderpx)
                         (* inventory-cells-x cell-w)))
(def inventory-height (+ (* 2 borderpx)
                          (* 2 cell-h)
                          (* inventory-cells-y cell-h)))

(def- inventoryrx (- screen-width inventory-width frame-screenborder-distance))
(def inventoryry frame-screenborder-distance)

(initialize
  (def inventory-frame (make-frame :name :inventory
                                   :bounds [inventoryrx
                                            inventoryry
                                            inventory-width
                                            inventory-height]
                                   :hotkey inventory-hotkey
                                   :visible false
                                   :parent ingamestate-display)))

(defn showing-player-inventory? [] (is-visible? inventory-frame))

(add-item-grid
  :grid-type :belt
  :w 3 ; add cells -> add hotkeys
  :h 1
  :rx (- screen-width (* 3 cell-w) 1) ; -1 because rendering exactly at screen-height the bottom&right line will not be seen
  :ry (- screen-height cell-h 1)
  :allows-type :usable
  :is-equipment-cell true
  :visible-check (constantly true))

(add-item-grid
  :grid-type :inventory
  :w inventory-cells-x
  :h inventory-cells-y
  :rx (+ inventoryrx borderpx)
  :ry (+ inventoryry borderpx (* 2 cell-h))
  :allows-type :all
  :is-equipment-cell false
  :visible-check showing-player-inventory?)

(defpreload ^:private background-icons {:torso (create-image "items/armorbg.png")
                                        :hands (create-image "items/handsbg.png")
                                        :implants (create-image "items/implantsbg.png")})

(add-item-grid
  :grid-type :hands
  :w 1
  :h 1
  :rx (+ inventoryrx borderpx)
  :ry (+ inventoryry borderpx)
  :allows-type :hands
  :is-equipment-cell true
  :visible-check showing-player-inventory?)

(add-item-grid
  :grid-type :torso
  :w 1
  :h 1
  :rx (+ inventoryrx borderpx cell-w)
  :ry (+ inventoryry borderpx)
  :allows-type :torso
  :is-equipment-cell true
  :visible-check showing-player-inventory?)

(add-item-grid
  :grid-type :implants
  :w 1
  :h 1
  :rx (+ inventoryrx borderpx)
  :ry (+ inventoryry borderpx cell-h)
  :allows-type :implant
  :is-equipment-cell true
  :visible-check showing-player-inventory?)

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
    (inc-count-of-item? (get-item cell) item)
    (cell-filled-and-allows-item? cell item)))

(defn- render-item-tooltip [g]
  (when-let [cell (get-mouseover-item-cell)]
    (when (get-item cell)
      (let [[rx ry] (get-cell-renderposi cell)
            textseq (get-item-textseq cell)]
        (if (= :belt (:grid-type cell))
          (apply render-readable-text g rx ry :above true textseq)
          (apply render-readable-text g rx (+ ry cell-h) textseq))))))

(ingame-loop-comp :item-tooltip
  (rendering :tooltips [g c]
    (render-item-tooltip g)))

(def- item-cells-bg-color (.darker background-color 0.5))
(def- item-cells-fg-color foreground-color)

(def- droppable-color (rgbcolor :g 0.6 :a 0.8))
(def- not-allowed-color (rgbcolor :r 0.6 :a 0.8))

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
            item (get-item cell)]
        (when (and (is-item-in-hand?) (= cell mouseover-item-cell))
          (render-cell-droppable-indicator g cell rx ry))
        (when item
          (render-item-in-cell g item cell (inc rx) (inc ry))) ; cell size > item-size so the item is not behind the grid lines
        (when (and (not item) (get background-icons grid-type))
          (draw-image (get background-icons grid-type) (inc rx) (inc ry)))))
    (set-color g item-cells-fg-color)
    (draw-grid g x y gridw gridh cell-w cell-h)))

(def hks-cells
  {:Q [0 0]
   :W [1 0]
   :E [2 0]})

(defn- render-belt-hotkeys [g]
  (doseq [hotkey (keys hks-cells)
          :let [cell-posi (get hks-cells hotkey)
                [rx ry] (get-cell-renderposi cell-posi :belt)]]
    (render-readable-text g (+ rx (/ cell-w 2)) ry :above true :centerx true (name hotkey))))

(ingame-loop-comp :item-cells
  (rendering [g c]
    (runmap
      #(when ((:visible-check %))
         (render-item-grid g (:grid-type %))) ; give grid as argument ...
      (vals item-grids))
    (render-belt-hotkeys g)))

(defn get-inventory-cells-with-item-name [item-name grid-type]
  (filter
    #(and
       (get-item %)
       (= (:name (get-item %)) item-name))
    (get-cells-from grid-type)))

(defn- try-put-item-in
  "returns true when the item was picked up"
  [picked-item grid-type]
  (let [item-cells (get-cells-from grid-type)
        cell-with-same-item (first
                              (get-inventory-cells-with-item-name (:name picked-item) grid-type))
        picked-up (if
                    (and
                      cell-with-same-item
                      (:count (get-item cell-with-same-item)))
                    (swap! (:item cell-with-same-item) update-in [:count] + (:count picked-item))
                    (if-let [free-cell (find-first empty-item-cell? item-cells)]
                      (do
                        (add-item-to-cell picked-item free-cell)
                        true)
                      false))]
    picked-up))

(defn try-pickup-item [item]
  (or
    (when-let [grid-type (:grid-type
                           (find-first #(= (:allows-type %) (:type item))
                             (vals item-grids)))]
      (try-put-item-in item grid-type)) ; 1. try
    (try-put-item-in item :inventory))) ; 2. try





