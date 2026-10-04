(ns game.utils.tilemap
  (:require
    [engine.input :refer [get-mouse-pos]]
    [game.maps.camera :refer [get-camera-position]]
    [utils.core :refer [int-posi]]
    [game.settings :refer [half-screen-h half-screen-w tile-height tile-width]]))

(defn tilepos-of-screenpos [[x y]]
  (let [[middlex middley] (get-camera-position)
        dist-to-middle-x (- x half-screen-w)
        dist-to-middle-y (- y half-screen-h)
        tile-x (+ middlex (/ dist-to-middle-x tile-width))
        tile-y (+ middley (/ dist-to-middle-y tile-height))]
    [tile-x tile-y]))

(defn screenpos-of-tilepos [[x y]]
    (let [[middlex middley] (get-camera-position)
          dist-to-middle-x (- x middlex)
          dist-to-middle-y (- y middley)
          screen-x (+ half-screen-w (* dist-to-middle-x tile-width))
          screen-y (+ half-screen-h (* dist-to-middle-y tile-height))]
      [screen-x screen-y]))

(defn get-mouse-tile-pos []
  (tilepos-of-screenpos (get-mouse-pos)))

(defn mouse-int-tile-pos []
  (int-posi (get-mouse-tile-pos)))
