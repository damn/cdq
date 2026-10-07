(ns game.monsters.little-bot
  (:require
    [engine.core :refer [create-sound play-sound]]
    [engine.render :refer [create-image]]
    [game.components.core :refer [get-id player-body]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [image-render-component]]
    [game.components.skills.melee :refer [monster-melee-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [death-trigger monster-die-effect]]))

(defmonster little-bot {:hp 0.5 :armor 0 :pxw 9 :pxh 9}
  (death-trigger (fn [body]
                   (play-sound "bfxr_defaultmonsterdeath.wav")
                   (monster-die-effect body)))
  (path-to-player-movement 15)
  (rotation-component)
  (image-render-component (create-image "opponents/littlebot.png"))
  (monster-melee-component
    :cooldown 1000
    :attacktime 100
    :hit-sound (create-sound "slash.wav")
    :target-id (get-id player-body)))
