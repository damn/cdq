(ns game.tests.circle-collides
  (:require [engine.render :as color]
            [game.utils.geom :as geom])
  (:use
    game.settings
    (game.components ingame-loop render core destructible)
    (game.utils tilemap raycast)
    (game.monster monsters)))

(def opponent (atom nil))

(reset! opponent (get-entity 511))

(def circle-radius 5)

(ingame-loop-comp :circle-collides
  (rendering :below-gui [g component]
    (when-let [entity @opponent]
      (let [posi (get-position entity)
            renderposi (screenpos-of-tilepos posi)
            pxradius (* circle-radius tile-width)
            circle (geom/circle renderposi pxradius)]
        (render-centered-shape g circle renderposi
          (if
            (some #(not (ray-blocked? posi (get-position %)))
              (get-destructible-bodies posi circle-radius :monster))
            color/red color/green))))))
;(remove-entity :circle-collides)
; (player-in-nova-range? (get-entity 441))

; ich glauben das ist oben links aufgebaut
