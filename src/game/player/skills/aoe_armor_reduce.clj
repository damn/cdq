(ns game.player.skills.aoe-armor-reduce
  (:require
    [game.components.body-effects-impl :refer [aoe-armor-reducer]]
    [game.components.skills.core :refer [get-skill-use-mouse-tile-pos]]
    [game.components.skills.utils :refer [check-line-of-sight]]
    [game.player.skill.learnable :refer [deflearnable-skill]]
    [game.player.skills.common :refer [create-curse-effect curse-infostr]]))

(deflearnable-skill aoe-armor-reduce
  :manacost 50
  :menu-posi [0 2]
  :mousebutton :both
  :icon "icons/armorreduce.png"
  :info (str curse-infostr "Reduces armor by 50%")
  {:radius 1.5
   :seconds 20
   :show-info-for [:cost :seconds]
   :animation :casting
   :check-usable check-line-of-sight
   :do-skill (fn [_ {:keys [radius seconds]}]
               (let [posi (get-skill-use-mouse-tile-pos)]
                 (create-curse-effect posi)
                 (aoe-armor-reducer posi radius 50 seconds)))})
