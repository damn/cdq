(ns game.tests.tiles-inside-rect
  (:require [engine.render :as color])
  (:use
    engine.input
    (game settings)
    (game.components render core ingame-loop)
    (game.utils geom tilemap)))

(def half-w 4)
(def half-h 4)

(def marked-tiles (atom nil))

(comment
  (ingame-loop-comp :tiles-inside-rect
    (rendering :below-gui [g component]
      (let [[x y] (get-mouse-pos)
            half-pxw (in-pixelx half-w)
            half-pxh (in-pixely half-h)
            pxw (* 2 half-pxw)
            pxh (* 2 half-pxh)]
        (draw-rect g (- x half-pxw) (- y half-pxh) pxw pxh color/red)))
    (active [delta _ _]
      (reset! marked-tiles
              (doall
                (tiles-inside-rect [(get-mouse-tile-pos) half-w half-h]))))))



