(ns game.utils.pathfinding
  (:require
    [game.utils.tilemap :refer [get-mouse-tile-pos]]
    [game.maps.cell-grid :refer [cell-blocked? get-cell get-map-h get-map-w]]
    [game.components.core :refer [get-position player-body]])
  (:import
    (org.newdawn.slick.util.pathfinding TileBasedMap AStarPathFinder Mover Path)))

(defn init-pathfinder []

  (def tilebasedmap ; initializes nodes to the getWidth/getHeight, so the current map get-map-w refers to
    (reify TileBasedMap
      (getHeightInTiles [this] (get-map-h))
      (getWidthInTiles [this] (get-map-w))
      (pathFinderVisited [this x y] false)
      (getCost [this context tx ty] (float 1))
      (blocked [this context tx ty] (boolean (cell-blocked? (get-cell [tx ty]) :ground)))))

  (def astarpathfinder
    (let [maxsearchdist 30
          allowdiagmovement true]
      (proxy [AStarPathFinder]
        [tilebasedmap maxsearchdist allowdiagmovement])))
  ; initialisiert die Nodes des pathfinder mit der width/height von der tilebasedmap
  ; d.h. eine gr�ssere map sp�ter f�hrt zu einer NPE
  ; -> f�r verschieden grosse maps verschiedene astarpathfinder erstellen!

  (def mover (reify Mover)))

(def current-path (atom nil))

; gibt indexoutofbounds exception falls maus position ausserhalb des grids ist
(defn update-current-path []
  (let [[sx sy] (get-position player-body)
        [tx ty] (get-mouse-tile-pos)]
    (reset!
      current-path
      (.findPath astarpathfinder mover sx sy tx ty))))



