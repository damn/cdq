(ns game.monsters.first-boss
  (:require
    [engine.core :refer [defpreload play-sound]]
    [engine.render.color :refer [white]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-animation folder-frames]]
    [utils.core :refer [translate-to-tile-middle]]
    [game.components.core :refer [add-to-removelist get-position player-body]]
    [game.components.body :refer [blocked-location?]]
    [game.components.render :refer [animation-entity create-line-render-effect single-animation-component]]
    [game.components.body-effects-impl :refer [dmg-effect stun-collision-effect]]
    [game.components.movement :refer [movement-component]]
    [game.components.skills.core :refer [standalone-skill]]
    [game.components.sleeping :refer [wake-up]]
    [game.entity.projectile :refer [fire-projectile]]
    [game.item.instance :refer [create-item-body]]
    [game.monster.defmonster :refer [defmonster get-monster-properties]]
    [game.monster.spawn :refer [try-spawn]]
    [game.utils.geom :refer [direction-vector get-touched-tiles]]
    [game.monsters.common :refer [big-body-hit-effect create-homing-movement death-trigger monsterteleport-animation]]))

(defn- rand-spawn-monster [monstertype position areahw areahh]
  (let [{:keys [half-w half-h]} (get-monster-properties monstertype)
        spawn-positions (remove #(blocked-location? % half-w half-h :ground) ; get-free-posis duplicate?
                                (map translate-to-tile-middle
                                     (get-touched-tiles position areahw areahh)))]
    (when (seq spawn-positions)
      (let [target (rand-nth spawn-positions)]
        (wake-up (try-spawn target monstertype)) ; TODO try-spawn also checks if blocked °_°
        (monsterteleport-animation target)
        (create-line-render-effect position target 70 :color white)))))

(defn- fire-boss-ranged-projectile [body speed starting-angle rotation-speed effects]
  (fire-projectile
    :startbody body
    :px-size 10
    :animation (folder-animation :folder "effects/bossball/" :duration 700 :looping true)
    :side :monster
    :hits-side :player
    :movement (create-homing-movement speed player-body starting-angle rotation-speed :air)
    :hit-effects effects
    :maxtime 12000))

(defpreload ^:private boss-explosion (folder-frames "effects/bossexplosion/"))

(defmonster first-boss {:hp 20 :armor 50 :pxw 33 :pxh 63}
  (big-body-hit-effect [[12 -15] [-8 -15] [0 -23] [1 -6] [-9 -4] [-8 4] [8 8] [-8 15] [3 23] [-6 25]])
  ;(light-component :color (rgbcolor :r 0.8 :g 0.2 :b 0.2) :intensity 1 :radius 12)
  (death-trigger (fn [body]
                   (play-sound "bfxr_bossdeath.wav")
                   (animation-entity
                     :animation (create-animation boss-explosion)
                     :position (get-position body))
                   (create-item-body (get-position body) "The Golden Banana")

                   ; no lvl after this => no need to spawn an item!
                   ; (create-rand-item (get-position body) :max-lvl (:rand-item-max-lvl (get-current-map-data)))

                   (dorun (map add-to-removelist (:projectiles (:boss-ranged @body))))))
  (standalone-skill
    :stype :monster-spawner
    :cooldown 2000
    :attacktime 500
    :props {:show-cast-bar true
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity component]
                        (rand-spawn-monster :little-bot (get-position entity) 6 3))})
  (standalone-skill
    :stype :boss-ranged
    :cooldown 3200
    :attacktime 3000
    :props {:show-cast-bar true
            :projectiles []
            :dmg [5 15]
            :shoot-sound "bfxr_monstercast.wav"
            :do-skill (fn [entity ranged-comp]
                        (let [speed 48 ; ca. player move speed
                              rotation-speed 0.05
                              effects [(dmg-effect (:dmg ranged-comp)) (stun-collision-effect 75 300)]]
                          (swap! entity update-in [:boss-ranged :projectiles] concat
                                      (doall
                                        (map #(fire-boss-ranged-projectile entity speed % rotation-speed effects)
                                             [0 90 180 270])))))})
  (movement-component ; TODO komische args ...
    {:control-update (fn [body _ _] (direction-vector (get-position body) (get-position player-body)))}
    12
    :ground)
  (single-animation-component ; TODO gleich folder-animation auchnoch reinpacken in single-animation-component?
    (folder-animation :folder "opponents/boss/" :duration 300 :looping true)))
