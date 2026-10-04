(ns game.utils.front-of-body-shape
  (:require
    [game.utils.geom :as geom]
    utils.core
    [game.components.core :refer [get-component get-half-width get-position]]
    game.components.body))

(defn in-front-of-body-shape
  ([body height]
   (in-front-of-body-shape (get-position body)
                           (get-half-width body)
                           height
                           (:angle (get-component body :rotation))))
  ([[x y] half-w height angle]
   (geom/rotate
    (geom/rectangle (- x half-w)
                    (- y half-w height)
                    (* 2 half-w)
                    height)
    angle x y)))
