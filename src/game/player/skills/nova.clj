(ns game.player.skills.nova
  (:require
    [engine.core :refer [defpreload]]
    [engine.render.animation :refer [create-animation]]
    [engine.render.assets :refer [folder-frames]]
    [game.components.core :refer [get-position]]
    [game.entity.nova :refer [nova-effect]]
    [game.player.skill.learnable :refer [deflearnable-skill]]
    [game.player.skills.common :refer [dmg-info-player-spell]]))

(defpreload ^:private nova-frames (folder-frames "effects/nova/"))

(deflearnable-skill nova
  :manacost 10
  :menu-posi [2 0]
  :mousebutton :right
  :icon "icons/nova.png"
  :info "Fires a nova."
  {:dmg [6 9]
   :dmg-info dmg-info-player-spell
   :radius 3
   :show-info-for [:cost :dmg]
   :animation :casting
   :do-skill (fn [entity {:keys [radius dmg] :as component}]
               (nova-effect
                 :position (get-position entity)
                 :duration 200
                 :maxradius radius
                 :affects-side :monster
                 :dmg dmg
                 :is-player-spell true
                 :animation (create-animation nova-frames)))})
