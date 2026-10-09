(ns game.monsters.xploding-drone
  (:require
    [engine.core :refer [create-sound play-sound]]
    [engine.render.assets :refer [folder-animation]]
    [game.components.core :refer [player-body]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [single-animation-component]]
    [game.components.skills.melee :refer [monster-melee-component]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.entity.nova :refer [nova-effect]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [death-trigger default-monster-death]]))

(defmonster xploding-drone {:hp 1.5 :armor 5 :pxw 15 :pxh 15}
  (death-trigger (fn [this-body]
                   (default-monster-death this-body :sound false) ; TODO this strange sound false and play-sound ... => default monster dead more than 1 thing...
                   (play-sound "bfxr_dronedeath.wav")
                   (nova-effect ; TODO two novas because different dmg to player/monster ... => im dealt dmg trigger berücksichtigen?
                     :position (:value (:position @this-body))
                     :duration 150
                     :maxradius 2
                     :affects-side [:player]
                     :dmg [20 20]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))
                   (nova-effect
                     :position (:value (:position @this-body))
                     :duration 150
                     :maxradius 2
                     :affects-side [:monster]
                     :dmg [4 8]
                     :animation (folder-animation :folder "effects/xpldrone/" :duration 150 :looping false))))
  (path-to-player-movement 13)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/xplodingdrone/" :duration 500 :looping true))
  (monster-melee-component
    :cooldown 500
    :attacktime 500
    :hit-sound (create-sound "slash.wav")
    :target-id (:id (meta player-body))))
