(ns game.monsters.ranged
  (:require
    [engine.render :refer [create-image]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.utils.random :refer [rand-int-between]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger ranged-component ranged-runaway-movement-comp]]))

(defmonster ranged {:hp 0.6 :armor 5 :pxw 15 :pxh 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 24 (rand-int-between 2 6) :ground)
  (rotation-component)
  (image-render-component (create-image "opponents/core_raider.png"))
  (ranged-component :cooldown (rand-int-between 2000 2500) :attacktime 500))
