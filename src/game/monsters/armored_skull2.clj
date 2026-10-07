(ns game.monsters.armored-skull2
  (:require
    [engine.render.image :refer [create-image]]
    [game.components.misc :refer [hp-regen-component rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger normal-monster-melee]]))

(defmonster armored-skull2 {:hp 1 :armor 23 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 5)
  (image-render-component (create-image "opponents/vorticularcutlass.png"))
  (normal-monster-melee))
