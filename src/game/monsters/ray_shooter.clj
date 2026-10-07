(ns game.monsters.ray-shooter
  (:require
    [engine.core :refer [play-sound]]
    [engine.render.color :refer [red white]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-animation spritesheet-frames]]
    [game.components.core :refer [get-position is-player? player-body]]
    [game.components.body :refer [get-bodies-at-position]]
    [game.components.render :refer [animation-entity create-line-render-effect single-animation-component]]
    [game.components.destructible :refer [deal-dmg]]
    [game.components.skills.core :refer [standalone-skill]]
    [game.components.movement.ai.potential-field :refer [path-to-player-movement]]
    [game.utils.geom :refer [in-range?]]
    [game.utils.random :refer [rand-int-between]]
    [game.utils.raycast :refer [ray-blocked?]]
    [game.monster.defmonster :refer [defmonster]]
    [game.monsters.common :refer [default-death-trigger teleport-and-heal-when-low-hp]]))

;TODO ray-blocked = in-sight -> cache it? expensive?
;TODO do-skill spritesheet-frames expensive without preloading the frames?
(let [maxrange-squared (* 10 10)
      attacktime 1000]
  (defmonster ray-shooter {:hp 1 :armor 25 :pxw 15 :pxh 15}
    (default-death-trigger)
    (path-to-player-movement 22)
    {:type :dealt-dmg-trigger :do teleport-and-heal-when-low-hp}
    (single-animation-component
      (folder-animation :folder "opponents/harvesterexoshield/" :duration 200 :looping true))
    (standalone-skill
      :stype :rayshoot
      :cooldown (rand-int-between 1700 2300)
      :attacktime attacktime
      :state-blocks {:attacking :movement} ; important because line rendered from these posis! so dont move when attacking!
      :props {:show-cast-bar false
              :shoot-sound "bfxr_rayshooterhit.wav"
              :target-posi (atom nil) ; REMOVE
              :check-usable (fn [entity component]
                              (let [shooter-posi (get-position entity)
                                    target-posi (get-position player-body)]
                                (when (and (in-range? shooter-posi target-posi maxrange-squared)
                                           (not (ray-blocked? shooter-posi target-posi)))
                                  (reset! (:target-posi component) target-posi)
                                  (play-sound "bfxr_powerup.wav") ; length of sound ~ length of attacktime would be nice
                                  ; TODO line render only as long as monster is alive would also make more sense ? ...
                                  ; also when slowed down ... attacktime changes ...
                                  ; => just like a component of an entity slowed down/lives with it
                                  (create-line-render-effect shooter-posi target-posi attacktime :color white :thin true)
                                  true)))
              :do-skill (fn [entity component]
                          (let [target @(:target-posi component)]
                            (if (some is-player? (get-bodies-at-position target))
                              (deal-dmg [10 15] player-body)
                              (animation-entity :position target
                                                :animation (create-animation (spritesheet-frames "effects/12_16_littleexpl.png" 12 16) :frame-duration 100)
                                                :order :on-ground))
                            (create-line-render-effect (get-position entity) target 70 :color red)))})))
