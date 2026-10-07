(ns game.monsters.burrower
  (:require
    [engine.render.image :refer [create-image]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    game.components.burrow
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger normal-monster-melee]]))

(defmonster burrower {:hp 2.5 :armor 20 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 30)
  (rotation-component)
  (image-render-component (create-image "opponents/burrower.png"))
  (normal-monster-melee)
  (game.components.burrow/burrow-component))
