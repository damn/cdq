(ns game.player.skills.bullettime-field
  (:require
    [game.components.body-effects-impl :refer [create-bullettime-effects]]
    [game.components.skills.core :refer [get-skill-use-mouse-tile-pos]]
    [game.components.skills.utils :refer [check-line-of-sight]]
    [game.player.skill.learnable :refer [deflearnable-skill]]
    [game.player.skills.common :refer [create-curse-effect curse-infostr]]))

(deflearnable-skill bullettime-field
  :manacost 50
  :menu-posi [0 1]
  :mousebutton :both
  :icon "icons/bullettime.png"
  :info (str curse-infostr "Slows down monsters by 60%")
  {:radius 1.5
   :seconds 20
   :show-info-for [:cost :seconds]
   :animation :casting
   :check-usable check-line-of-sight
   :do-skill (fn [_ {:keys [radius seconds]}]
               (let [posi (get-skill-use-mouse-tile-pos)]
                 (create-curse-effect posi)
                 (create-bullettime-effects posi radius seconds)))})
