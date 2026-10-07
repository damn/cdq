(ns game.monsters.littlespider
  (:require
    [engine.render :refer [folder-animation]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [single-animation-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger normal-monster-melee]]))

(defmonster littlespider {:hp 1 :armor 7 :pxw 13 :pxh 13}
  (default-death-trigger)
  (path-to-player-movement 47)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/littlespider/" :duration 300 :looping true))
  (normal-monster-melee))
