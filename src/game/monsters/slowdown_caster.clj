(ns game.monsters.slowdown-caster
  (:require
    [engine.render.assets :refer [folder-animation]]
    [game.components.core :refer [player-body]]
    [game.components.render :refer [single-animation-component]]
    [game.components.body-effects-impl :refer [dmg-effect slowdown-effect]]
    [game.components.skills.core :refer [standalone-skill]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.entity.projectile :refer [fire-projectile]]
    [game.utils.geom :refer [get-angle-to-position]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [create-homing-movement default-death-trigger]]))

(defmonster slowdown-caster {:hp 2 :armor 25 :pxw 12 :pxh 12}
  (default-death-trigger)
  (path-to-player-movement 20)
  (single-animation-component
    (folder-animation :folder "opponents/slowdowncaster/" :duration 200 :looping true))
  (standalone-skill        ; TODO für nen ranged skill mit custom hit-effects & movement sehr kompliziert!!
    :stype :slowdown-ranged
    :cooldown 2000
    :attacktime 1000
    :props {:show-cast-bar true
            :do-skill (fn [entity ranged-comp]
                        (let [speed 84
                              rotation-speed 0.1
                              starting-angle (get-angle-to-position (:value (:position @entity)) (:value (:position @player-body)))]
                          (fire-projectile
                            :startbody entity
                            :px-size 10
                            :animation (folder-animation :folder "effects/slowdownprojectile/" :duration 700 :looping true)
                            :side :monster
                            :hits-side :player
                            :movement (create-homing-movement speed player-body starting-angle rotation-speed :air)
                            :hit-effects [(dmg-effect [5 6])
                                          (slowdown-effect 1)]
                            :maxtime 16000)))}))
