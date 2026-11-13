(ns game.tests.circle-collides
  (:require
    [cdq.core :refer [circle]]
    [cdq.core :refer [green red]]
    [cdq.core :refer [tile-width]]
    [cdq.core :refer [ingame-loop-comp]]
    [game.components.render :refer [rendering]]
    [cdq.core :refer [id-entity-map]]
    [game.components.destructible :refer [get-destructible-bodies]]
    [cdq.core :refer [screenpos-of-tilepos]]
    [cdq.core :refer [ray-blocked?]]))
; monsters live in game.start

(def opponent (atom nil))

(reset! opponent (get @id-entity-map 511))

(def circle-radius 5)

(ingame-loop-comp :circle-collides
  (rendering :below-gui [g component]
    (when-let [entity @opponent]
      (let [posi (:value (:position @entity))
            renderposi (screenpos-of-tilepos posi)
            pxradius (* circle-radius tile-width)
            circle (circle renderposi pxradius)]
        (render-centered-shape g circle renderposi
          (if
            (some #(not (ray-blocked? posi (:value (:position @%))))
              (get-destructible-bodies posi circle-radius :monster))
            red green))))))
;(remove-ingame-loop-entity :circle-collides)
; (player-in-nova-range? (get-entity 441))

; ich glauben das ist oben links aufgebaut
