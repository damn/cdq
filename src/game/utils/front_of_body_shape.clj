(ns game.utils.front-of-body-shape
  (:require
    [game.utils.geom :as geom]
    utils.core
    game.components.body))

(defn in-front-of-body-shape
  ([body height]
   (in-front-of-body-shape (:value (:position @body))
                           (:half-width (:body @body))
                           height
                           (:angle (:rotation @body))))
  ([[x y] half-w height angle]
   (geom/rotate
    (geom/rectangle (- x half-w)
                    (- y half-w height)
                    (* 2 half-w)
                    height)
    angle x y)))
