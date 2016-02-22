(ns game.maps.tiledmaps
  (:use
    data.grid2d
    [game.maps.cell-grid :only (create-cell)])
  (:import (org.newdawn.slick.tiled TiledMap LightManager)))

(defn construct-tiledmap
  ([file]
   (TiledMap. ^String file))
  ([file lightmanager]
   (TiledMap. ^String file ^LightManager lightmanager)))

(defn get-layer-index [tiled-map layername]
  (let [idx (.getLayerIndex ^TiledMap tiled-map layername)]
    (when-not (= -1 idx) idx)))

; Conversion of TiledMap to cell-grid

(defn- get-blocks-property [^TiledMap tiled-map [x y] layer-index]
  (let [gid (.getTileId tiled-map x y layer-index)
        undefined (zero? gid)]
    (when-not undefined ; no tile placed at this location
      (case (.getTileProperty tiled-map gid "walkable" "default")
        "air" :airwalkable
        "ground" :ground
        "default" :wall))))

(defn create-grid-from-tiled-map [^TiledMap tiled-map]
  (let [ground-layer-idx (get-layer-index tiled-map "ground")
        details-layer-idx (get-layer-index tiled-map "details")]
    (create-grid (.getWidth tiled-map)
                 (.getHeight tiled-map)
                 (fn [p]
                   (or
                     (when details-layer-idx
                       (get-blocks-property tiled-map p details-layer-idx))
                     (get-blocks-property tiled-map p ground-layer-idx))))))

(defn create-cell-grid-from-tiled-map [tiled-map]
  (transform (create-grid-from-tiled-map tiled-map)
             (fn [p value]
               (when value
                 (create-cell p (case value
                                  :ground #{}
                                  :wall #{:air :ground}
                                  :airwalkable #{:ground}))))))

;;

; .getTileProperty returns The value assigned to the property on the tile (or the default value if none is supplied)
(defn get-tile-property-value [[x y] ^TiledMap tiled-map layer-idx property]
  (let [gid (.getTileId tiled-map x y layer-idx)
        value (.getTileProperty tiled-map gid property "def")]
    (when-not (= "def" value)
      value)))

(defn get-tileproperties [^TiledMap tiled-map layername property]
  (when-let [idx (get-layer-index tiled-map layername)]
    (for [x (range (.getWidth tiled-map))
          y (range (.getHeight tiled-map))
          :let [value (get-tile-property-value [x y] tiled-map idx property)]
          :when value] ; dont use when-let because then nil's will be in the result sequence
      [[x y] value])))

(defn get-player-entity-start-position [tiled-map]
  (let [tile-values (get-tileproperties tiled-map "entities" "player")]
    (cond (empty? tile-values) (throw (Error. "Player start position not found in \"entities\" layer of tiledmap."))
          (= (count tile-values) 1) (first (first tile-values))
          :else (throw (Error. "Multiple player start positions found in \"entities\" layer of tiledmap.")))))

