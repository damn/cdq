(ns game.utils.front-of-body-shape
  (:require [game.utils.geom :as geom])
  (:use utils.core
        (game.components core body)))

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
