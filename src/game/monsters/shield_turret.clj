(ns game.monsters.shield-turret
  (:require
    [engine.render.image :refer [create-image]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.shield :refer [shield-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger ranged-component]]))

(defmonster shield-turret {:hp 1.5 :armor 15 :pxw 15 :pxh 15}
  (path-to-player-movement 13)
  (default-death-trigger)
  (shield-component 1500)
  (rotation-component)
  (image-render-component (create-image "opponents/counternegativeenergyturret.png"))
  (ranged-component :cooldown 4000 :attacktime 500))
