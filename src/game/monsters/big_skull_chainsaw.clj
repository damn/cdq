(ns game.monsters.big-skull-chainsaw
  (:require
    [engine.render :refer [create-image]]
    [game.components.misc :refer [hp-regen-component rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger normal-monster-melee]]))

(defmonster big-skull-chainsaw {:hp 3 :armor 25 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 25)
  (rotation-component)
  (hp-regen-component 1)
  (image-render-component (create-image "opponents/vorticularcutlassiv.png"))
  (normal-monster-melee))
