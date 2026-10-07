(ns game.player.skills.player-ranged
  (:require
    [engine.core :refer [defpreload]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-frames]]
    [game.components.body-effects-impl :refer [dmg-effect stun-collision-effect]]
    [game.components.movement :refer [projectile-movement-component]]
    [game.components.skills.utils :refer [get-player-ranged-vector]]
    [game.entity.projectile :refer [fire-projectile]]
    [game.player.skill.learnable :refer [deflearnable-skill]]
    [game.player.skills.common :refer [dmg-info-player-spell]]))

(defpreload ^:private projectile-frames (folder-frames "effects/energyball/"))

(deflearnable-skill player-ranged
  :manacost 2
  :menu-posi [0 0]
  :mousebutton :both
  :icon "icons/ranged.png"
  :info "Fires a projectile"
  {:dmg [5 7]
   :dmg-info dmg-info-player-spell
   :show-info-for [:cost :dmg]
   :animation :casting
   :do-skill (fn [entity component]
               (fire-projectile
                 :startbody entity
                 :px-size 8
                 :animation (create-animation projectile-frames :looping true)
                 :side :player
                 :hits-side :monster
                 :movement (projectile-movement-component (get-player-ranged-vector) 160)
                 :hit-effects [(dmg-effect (:dmg component) :is-player-spell true)
                               (stun-collision-effect 100 200)]
                 :piercing false
                 :maxrange 8))})
