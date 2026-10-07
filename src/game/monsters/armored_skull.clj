(ns game.monsters.armored-skull
  (:require
    [engine.render.image :refer [create-image]]
    [game.components.misc :refer [hp-regen-component rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger normal-monster-melee]]))

(defmonster armored-skull {:hp 2 :armor 65 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 13)
  (rotation-component)
  (hp-regen-component 2)
  (image-render-component (create-image "opponents/coredemonhand.png"))
  (normal-monster-melee))
