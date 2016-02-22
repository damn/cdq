(ns game.maps.camera
  (:use
    [game.components.core :only (get-position player-body)]))

(defn get-camera-position
  "returns the current center-of-screen-map-tile-position.
Rendering the map, minimap, map-entities depends on the camera position"
  []
  (get-position player-body))