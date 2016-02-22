(ns game.maps.render
  (:require [game.debug-settings :as debug]
            [engine.render :as color]
            (game.maps add [data :as data])
            game.utils.lightning)
  (:use [utils.core :only (def-)]
        engine.render
        (game settings [mouseoverbody :only (get-mouseover-body)])
        (game.maps cell-grid camera tiledmaps)
        game.tests.tiles-inside-rect
        game.components.movement.ai.potential-field)
  (:import (org.newdawn.slick Image SpriteSheet)
           org.newdawn.slick.tiled.TiledMap))

(defn- render-generated-grid [x y cells ^SpriteSheet sprite-sheet get-sprite-posi]
  (.startUse sprite-sheet)
  (doseq [[tx ty cell tileposi] cells]
    (when-let [sheet-posi (get-sprite-posi @cell)]
      (let [image (get-sprite sprite-sheet sheet-posi)
            render-x (+ x (* tile-width tx))
            render-y (+ y (* tile-height ty))]
        (game.utils.lightning/set-cached-brightness image tileposi)
        (.drawEmbedded ^Image image render-x render-y tile-width tile-height))))
  (.endUse sprite-sheet))

(defn- render-gen-grids [x y sx sy width height]
  (let [grid (get-cell-grid)
        cells (for [tx (range width)
                    ty (range height)
                    :let [tileposi [(+ sx tx) (+ sy ty)]
                          cell (get grid tileposi)]
                    :when cell]
                [tx ty cell tileposi])
        {:keys [sprite-sheet details-sprite-sheet]} (data/get-current-map-data)]
    (render-generated-grid x y cells sprite-sheet :sprite-posi)
    (when details-sprite-sheet
      (render-generated-grid x y cells details-sprite-sheet :details-sprite-posi))))

(def- marker-size 12)
(def- pathfnd-marker-size 8)

(defn- render-debug-map-info
  [g render-tile-start-x render-tile-start-y render-start-x render-start-y]
  (let [xrange (range -1 (+ display-width-in-tiles 3))
        yrange (range -1 (+ display-height-in-tiles 3))
        half-tile-width (/ tile-width 2)
        half-tile-height (/ tile-height 2)
        half-marker-size (/ marker-size 2)
        mouseoverbody (get-mouseover-body)]
    (when debug/potential-field-following-mouseover-info
      (calculate-mouseover-body-colors mouseoverbody))
    (doseq [x xrange
            y yrange
            :let [tilex (+ x render-tile-start-x)
                  tiley (+ y render-tile-start-y)
                  cell (get-cell [tilex tiley])
                  corner-x (+ render-start-x (* x tile-width))
                  corner-y (+ render-start-y (* y tile-height))
                  xrect (- (+ corner-x half-tile-width) half-marker-size)
                  yrect (- (+ corner-y half-tile-height) half-marker-size)]
            :when cell]
      (when
        (and
          debug/show-besetzte-cells
          (seq (get-body-ids cell)))
        (fill-rect g xrect yrect marker-size marker-size color/yellow))
      (when debug/show-blocked-cells
        (when (cell-blocked? cell :ground)
          (fill-rect g xrect yrect marker-size marker-size color/red)))
      (when
        (and
          debug/show-occupied-cells
          (seq (:occupied (deref cell))))
        (fill-rect g xrect yrect marker-size marker-size color/yellow))
      (when debug/potential-field-following-mouseover-info
        (render-potential-field-following-mouseover-info g corner-x corner-y xrect yrect cell mouseoverbody))
      (when debug/show-potential-field
        (render-potential-field-info g corner-x corner-y xrect yrect cell))
      ; :translated bedeutet hier: translate nicht wie on-screen den text.
      (comment
        (when
          (and
            @current-path
            (.contains ^Path @current-path tilex tiley))
          (fill-rect g xrect yrect pathfnd-marker-size pathfnd-marker-size color/red)))
      ; tiles-inside-rect debug test -> wie die ganzen tests bei debug an/ausschalten w�hrend game l�uft?
      (comment
        (when (some #{[tilex tiley]} @marked-tiles)
          (fill-rect g xrect yrect pathfnd-marker-size pathfnd-marker-size color/red))
        (fill-rect g corner-x corner-y 4 4 color/orange))
      (comment
        (when
          (and
            @current-steplist
            (steplist-contains? tilex tiley))
          (fill-rect g xrect yrect pathfnd-marker-size pathfnd-marker-size color/black))))
    (when debug/show-map-grid
      (set-color g color/black)
      (draw-grid g
                 (- render-start-x (* render-tile-start-x tile-width))
                 (- render-start-y (* render-tile-start-y tile-height))
                 (get-map-w)
                 (get-map-h)
                 tile-width
                 tile-height))))

; left-offset-in-tiles-buffer und top-offset-in-tiles-buffer sind eine Korrektion
; bei der Umrechnung von int in float bei width und heightInTiles
(def left-offset-in-tiles-buffer (- left-offset-in-tiles half-display-w-in-tiles))
(def top-offset-in-tiles-buffer (- top-offset-in-tiles half-display-h-in-tiles))

; sx = start-x; tsx = tile-start-x
(defn rendermap [g]
  "Zeichnet die map um einen center-point der eine float-tileposition ist. center-position in tiles."
  (let [[center-x center-y] (get-camera-position)
        center-tile-x (int center-x)
        center-tile-y (int center-y)
        ; caculate the offset of the center-point from the edge of the tile.
        ; As the center-point moves around this
        ; varies and this tells us how far to offset the tile based rendering
        ; to give the smooth
        ; motion of scrolling
        center-tile-offset-x (int (* tile-width (- center-tile-x center-x)))
        center-tile-offset-y (int (* tile-height (- center-tile-y center-y)))
        sx (- center-tile-offset-x (int (* left-offset-in-tiles-buffer tile-width)) tile-width)
        sy (- center-tile-offset-y (int (* top-offset-in-tiles-buffer tile-height)) tile-height)
        tsx (dec (- center-tile-x left-offset-in-tiles))
        tsy (dec (- center-tile-y top-offset-in-tiles))
        render-width-tiles (+ display-width-in-tiles 3)
        render-height-tiles (+ display-height-in-tiles 3)]
    (if-let [^TiledMap tiled-map (:tiled-map (data/get-current-map-data))]
      (do
        (.render tiled-map sx sy tsx tsy render-width-tiles render-height-tiles (get-layer-index tiled-map "ground") false)
        (when-let [idx (get-layer-index tiled-map "details")]
          (.render tiled-map sx sy tsx tsy render-width-tiles render-height-tiles idx false)))
      (render-gen-grids sx sy tsx tsy render-width-tiles render-height-tiles))
    (when @debug-mode
      (render-debug-map-info g tsx tsy sx sy))))

