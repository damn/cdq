(ns game.item.grids
  (:require
    [engine.render :refer [create-image]]
    [engine.core :refer [defpreload initialize]]
    [game.settings :refer [screen-height screen-width]]
    [game.ingame-gui :refer [frame-screenborder-distance ingamestate-display inventory-hotkey is-visible? make-frame]]
    [data.grid2d :refer [cells create-grid height width]]))

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

(defn add-item-grid [& {:keys [w h rx ry allows-type grid-type is-equipment-cell visible-check]
                        :as argsmap}]
  {:pre [(not-any? #{grid-type} (keys item-grids))]}
  (alter-var-root #'item-grids assoc grid-type
                  (ItemGrid. (create-grid w h #(create-empty-item-cell % allows-type grid-type is-equipment-cell))
                             (select-keys argsmap [:visible-check :grid-type :rx :ry :allows-type]))))

(def ^:private inventory-cells-x 6)
(def ^:private inventory-cells-y 4)

(def cell-w 17) ; cells = item-size+1 so items fit inside grid lines, else top and left pixel line of items not visible
(def cell-h 17)

(def ^:private borderpx 2)
(def ^:private inventory-width (+ (* 2 borderpx)
                         (* inventory-cells-x cell-w)))
(def inventory-height (+ (* 2 borderpx)
                          (* 2 cell-h)
                          (* inventory-cells-y cell-h)))

(def ^:private inventoryrx (- screen-width inventory-width frame-screenborder-distance))
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

(defpreload background-icons {:torso (create-image "items/armorbg.png")
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

(def hks-cells
  {:Q [0 0]
   :W [1 0]
   :E [2 0]})
