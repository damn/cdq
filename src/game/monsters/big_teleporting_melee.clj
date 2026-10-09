(ns game.monsters.big-teleporting-melee
  (:require
    [engine.render.color :refer [white]]
    [engine.render.assets :refer [folder-animation]]
    [game.components.core :refer [player-body]]
    [game.components.body :refer [get-dist-to-player teleport]]
    [game.components.misc :refer [rotation-component]]
    [game.components.render :refer [create-line-render-effect single-animation-component]]
    [game.components.skills.core :refer [standalone-skill]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger get-free-posis monsterteleport-animation normal-monster-melee]]))

(defmonster big-teleporting-melee {:hp 5 :armor 15 :pxw 15 :pxh 15}
  (default-death-trigger)
  (path-to-player-movement 8)
  (rotation-component)
  (single-animation-component
    (folder-animation :folder "opponents/corebomber/" :duration 300 :looping true))
  (normal-monster-melee)
  (standalone-skill
    :stype :teleporting
    :cooldown 1000
    :attacktime 600
    :props {:show-cast-bar true
            :shoot-sound "bfxr_bigmeleeteleport.wav"
            :check-usable (fn [entity _]
                            (let [dist (get-dist-to-player entity)]
                              (or (not dist) (>= dist 80))))
            :do-skill (fn [entity component]
                        (let [old-posi (:value (:position @entity))
                              posis (get-free-posis entity (:value (:position @player-body)) 2 2)]
                          (when (not-empty posis)
                            (let [posi (rand-nth posis)]
                              (teleport entity posi)
                              (monsterteleport-animation posi)
                              (create-line-render-effect posi old-posi 140 :color white)))))}))
; TODO shoot-sound?
