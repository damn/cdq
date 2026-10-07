(ns game.monsters.skull-chainsaw
  (:require
    [engine.render.assets :refer [folder-animation]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [single-animation-component]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger lowhp-dealt-dmg-trigger lowhp-runaway-movement normal-monster-melee]]))

(defmonster skull-chainsaw {:hp 1.3 :armor 4 :pxw 15 :pxh 15}
  (default-death-trigger)
  (lowhp-runaway-movement 50)
  {:type :dealt-dmg-trigger :do lowhp-dealt-dmg-trigger}
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/teleportraider/" :duration 500 :looping true))
  (normal-monster-melee))
