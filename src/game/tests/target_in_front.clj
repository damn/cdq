(ns game.tests.target-in-front
  (:require [engine.render :as color])
  (:use
    (engine render)
    game.settings
    (game.components core body render)
    (game.utils front-of-body-shape)
    game.components.skills.melee))

(defn- make-render-shape [body]
  (let [posi (-> body get-position translate-position)
        hbodyw (get-half-pxw body)
        height (* melee-puffer tile-width)]
    (in-front-of-body-shape posi hbodyw height (:angle (get-component body :rotation)))))

(defn- render-it [g body c]
  (set-color g (if (get-attackable-target-in-front body) color/red color/green))
  (draw-shape g (make-render-shape body)))

(defcomponent target-in-front-rect-render []
  (render-on-map :air [g entity c render-posi]
                 (render-it g entity c)))








