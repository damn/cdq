(ns game.monsters.mage-skull
  (:require
    [engine.render :refer [create-image]]
    [game.components.misc :refer [hp-regen-component rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.utils.random :refer [rand-int-between]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger ranged-component ranged-runaway-movement-comp]]))

(defmonster mage-skull {:hp 4.8 :armor 0 :pxw 15 :pxh 15}
  (default-death-trigger)
  (ranged-runaway-movement-comp 26 (rand-int-between 3 4) :ground)
  (hp-regen-component 5)
  (rotation-component)
  (image-render-component (create-image "opponents/core_predator.png"))
  (ranged-component :cooldown 3000 :attacktime 50))
